/*
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <http://unlicense.org>
*/

#include <algorithm>
#include <memory>

#include "eop_parser.h"
#include "eop_ast.h"

using std::string;
using std::string_view;

std::vector<string> template_names;

/*
Syntax Notation

An Extended Backus-Naur Form designed by Niklaus Wirth is used. Wirth
[1977, pages 822–823] describes it as follows:

    The word identifier is used to denote nonterminal symbol, and
    literal stands for terminal symbol. For brevity, identifier and
    character are not defined in further detail.

    syntax = {production}.
    production = identifier "=" expression ".".
    expression = term {"|" term}.
    term = factor {factor}.
    factor = identifier | literal
                | "(" expression ")"
                | "[" expression "]"
                | "{" expression "}".
    literal = """" character {character} """".

    Repetition is denoted by curly brackets, i.e. {a} stands for 0 | a
    | aa | aaa | .... Optionality is expressed by square brack-
    ets, i.e. [a] stands  for a | 0. Parentheses merely serve for
    grouping, e.g.(a | b) c stands for ac | bc. Terminal sym-
    bols, i.e. literals, are enclosed in quote marks (and, if a quote
    mark appears as a literal itself, it is written twice).
*/

// identifier = (letter | "_") {letter | "_" | digit}.
auto eop_identifier(string_view input) -> Parsed_t<Eop>
{
    return pass_empty(
        choice(
            str("case"),
            str("const"),
            str("do"),
            str("else"),
            str("enum"),
            str("false"),
            str("goto"),
            str("if"),
            str("operator"),
            str("requires"),
            str("return"),
            str("struct"),
            str("switch"),
            str("template"),
            str("true"),
            str("typedef"),
            str("typename"),
            str("while")
        ),
        sequence(
            [](auto const& ch, auto const& str)
            {
                return Eop{Identifier{string(1, ch) + str}};
            },
            choice(letter, symbol('_')),
            many(choice(letter, symbol('_'), digit))
        )
    )(input);
}

// literal = boolean | integer | real.
auto eop_literal(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& literal)
        {
            return Eop{Literal{literal}};
        },
         choice(
            eop_real,
            eop_integer,
            eop_boolean
        )
    )(input);
}

// boolean = "false" | "true".
auto eop_boolean(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& str)
        {
            return Eop{Boolean{str == "true"}};
        },
        choice(str("false"), str("true"))
    )(input);
}

// integer = digit {digit}.
auto eop_integer(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& str)
        {
            return Eop{Integer{std::stoi(str)}};
        },
        some(digit)
    )(input);
}

// real = integer "." [integer] | "." integer.
auto eop_real(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& str)
        {
            return Eop{Real{std::stod(str)}};
        },
        choice(
            sequence(
                [](auto const& x, auto, auto const y){ return fmt(x) + "." + (y ? fmt(*y) : string{}); },
                eop_integer,
                symbol('.'),
                maybe(eop_integer)
            ),
            sequence(
                [](auto, auto const& x){ return string{"."} + fmt(x); },
                symbol('.'),
                eop_integer
            )
        )
    )(input);
}

/*
Comments extend from two slashes to the end of the line:
*/

// comment = "//" {character} eol.
auto eop_comment(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto const& str, auto)
        {
            return Eop{Comment{str}};
        },
        str("//"),
        many(character),
        eol
    )(input);
}

/*
Basic Types

Three C++ types are used: bool has values false and true, int has signed
integer values, and double has IEEE 64-bit floating-point values:
*/

// basic_type = "bool" | "int" | "double".
auto eop_basic_type(string_view input) -> Parsed_t<Eop>
{
    return choice(
        sequence([](auto const&){ return Eop{Basic_type{Basic_type::Type::bool_v}}; }, str("bool")),
        sequence([](auto const&){ return Eop{Basic_type{Basic_type::Type::int_v}}; }, str("int")),
        sequence([](auto const&){ return Eop{Basic_type{Basic_type::Type::double_v}}; }, str("double"))
    )(input);
}

/*
Expressions

Expressions may be either runtime or compile time. Compile-time expres-
sions may evaluate to either a value or a type.
Expressions are defined by the following grammar. Operators in inner
productions—those appearing lower in the grammar—have a higher order
of precedence than those in outer productions:

The || and && operators designate ∨(disjunction) and ∧(conjunction),
respectively. The operands must be Boolean values. The first operand is
evaluated prior to the second operand. If the first operand determines the
outcome of the expression (true for ||, or false for &&), the second operand
is not evaluated, and the result is the value of the first operand. Prefix ! is
¬(negation) and must be applied to a Boolean value.
== and != are, respectively, equality and inequality operators and return
a Boolean value. <, >, <=, and >= are, respectively, less than, greater than, less or equal,
and greater or equal, also returning a Boolean value.
+ and - are, respectively, addition and subtraction; prefix - is additive
inverse.
*, /, and % are, respectively, multiplication, division, and remainder.
Postfix . (dot) takes an object of structure type and returns the mem-
ber corresponding to the identifier following the dot.
Postfix() takes  aprocedure or object on which the apply operator is defined and returns the
result of invoking the procedure or function object with the given argu-
ments. When applied to a type, () performs a construction using the given
arguments; when applied to a type function, it returns another type. Postfix
[] takes an object on which the index operator is defined and returns the
element whose position is determined by the value of the expression within
the brackets.
Prefix const is a type operator returning a type that is a constant version
of its operand. When applied to a reference type, the resulting type is a
reference to a constant version of the reference base type.
Postfix & is a type operator returning a reference type of its operand.
*/

// expression = conjunction {"||" conjunction}.
auto eop_expression(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& conjunction, auto const& conjunctions)
        {
            return Eop{Expression{conjunction, conjunctions}};
        },
        eop_conjunction,
        repeat(chain(token(str("||")), [](auto){ return eop_conjunction; }))
    )(input);
}

// conjunction = equality {"&&" equality}.
auto eop_conjunction(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& expression, auto const& expressions)
        {
            return Eop{Conjunction{expression, expressions}};
        },
        eop_equality,
        repeat(chain(token(str("&&")), [](auto){ return eop_equality; }))
    )(input);
}

// equality = relational {("==" | "!=") relational}.
auto eop_equality(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& relational, auto const& relationals)
        {
            return Eop{Equality{relational, relationals}};
        },
        eop_relational,
        repeat(
            choice(
                op_pair("==", eop_relational, Equality::Type::eq_v),
                op_pair("!=", eop_relational, Equality::Type::ne_v)
            )
        )
    )(input);
}

// relational = additive {("<" | ">" | "<=" | ">=") additive}.
auto eop_relational(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& additive, auto const& additives)
        {
            return Eop{Relational{additive, additives}};
        },
        eop_additive,
        repeat(
            choice(
                op_pair("<", eop_additive, Relational::Type::lt_v),
                op_pair(">", eop_additive, Relational::Type::gt_v),
                op_pair("<=", eop_additive, Relational::Type::le_v),
                op_pair(">=", eop_additive, Relational::Type::ge_v)
            )
        )
    )(input);
}

// additive = multiplicative {("+" | "-") multiplicative}.
auto eop_additive(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& multiplicative, auto const& multiplicatives)
        {
            return Eop{Additive{multiplicative, multiplicatives}};
        },
        eop_multiplicative,
        repeat(
            choice(
                op_pair("+", eop_multiplicative, Additive::Type::plus_v),
                op_pair("-", eop_multiplicative, Additive::Type::minus_v)
            )
        )
    )(input);
}

// multiplicative = prefix {("*" | "/" | "%") prefix}.
auto eop_multiplicative(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& index, auto const& indexes)
        {
            return Eop{Multiplicative{index, indexes}};
        },
        eop_prefix,
        repeat(
            choice(
                op_pair("*", eop_prefix, Multiplicative::Type::mul_v),
                op_pair("/", eop_prefix, Multiplicative::Type::quot_v),
                op_pair("%", eop_prefix, Multiplicative::Type::rem_v)
            )
        )
    )(input);
}

// prefix = ["-" | "!" | "const"] postfix.
auto eop_prefix(string_view input) -> Parsed_t<Eop>
{
   return sequence(
        [](auto const& value)
        {
            return Eop{Prefix{value}};
        },
        choice(
            op_pair("-", eop_postfix, Prefix::Type::neg_v),
            op_pair("!", eop_postfix, Prefix::Type::not_v),
            op_pair("const ", eop_postfix, Prefix::Type::const_v),
            op_pair("", eop_postfix, Prefix::Type::none_v)
        )
    )(input);
}

auto eop_ref(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const&)
        {
            return Eop{Ref{}};
        },
        pass_empty(token(str("&&")), token(symbol('&')))
    )(input);
}

// postfix = primary {"." identifier | "(" [expression_list] ")" | "[" expression "]" | "&"}.
auto eop_postfix(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& primary, auto const& post)
        {
            return Eop{Postfix{primary, post}};
        },
        eop_primary,
        repeat(
            choice(
                sequence(
                    [](auto, auto const& identifier){ return std::pair{Postfix::Type::id_v, identifier}; },
                    token(symbol('.')),
                    eop_identifier
                ),
                sequence(
                    [](auto, auto const& expression_list, auto){ return std::pair{Postfix::Type::exprs_v, Eop{expression_list}}; },
                    token(symbol('(')),
                    maybe(eop_expression_list),
                    token(symbol(')'))
                ),
                sequence(
                    [](auto, auto const& expression, auto){ return std::pair{Postfix::Type::index_v, expression}; },
                    token(symbol('[')),
                    eop_expression,
                    token(symbol(']'))
                ),
                sequence(
                    [](auto const& ref){ return std::pair{Postfix::Type::ref_v, ref}; },
                    eop_ref
                )
            )
        )
    )(input);
}

auto eop_typename(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const&)
        {
            return Eop{Typename{}};
        },
        str("typename")
    )(input);
}

// primary = literal | identifier | "(" expression ")" | basic_type | template_name | "typename".
auto eop_primary(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& x)
        {
            return Eop{Primary{x.first, x.second}};
        },
        choice(
            sequence(
                [](auto const& x){ return std::pair{x, false}; },
                eop_typename
            ),
            sequence( // See Templates (2.)
                [](auto const& x){ return std::pair{x, false}; },
                [](string_view inp) -> Parsed_t<Eop>
                {
                    auto const pos = std::find_if(
                        std::cbegin(template_names),
                        std::cend(template_names),
                        [inp](auto const& st){ return inp.starts_with(st.c_str()); }
                    );
                    if (pos != std::cend(template_names)) {
                       return {{Eop{Identifier{*pos}}, inp.substr(pos->size())}};
                    } else {
                        return {};
                    }
                }
            ),
            sequence(
                [](auto const& x){ return std::pair{x, false}; },
                eop_basic_type
            ),
            sequence(
                [](auto, auto const& x, auto){ return std::pair{x, true}; },
                symbol('('),
                token(eop_expression),
                symbol(')')
            ),
            sequence(
                [](auto const& x){ return std::pair{x, false}; },
                eop_literal
            ),
            sequence(
                [](auto const& x){ return std::pair{x, false}; },
                eop_identifier
            )
        )
    )(input);
}

// expression_list = expression {"," expression}.
auto eop_expression_list(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& expression, auto const& expressions)
        {
            return Eop{Expression_list{expression, expressions}};
        },
        eop_expression,
        repeat(chain(token(symbol(',')), [](auto){ return eop_expression; }))
    )(input);
}

/*
Enumerations

An enumeration generates a type with a unique value corresponding to each
identifier in the list. The only operations defined on enumerations are those
of regular types:  equality, relational operations, inequality, construction,
destruction, and assignment:
*/

// enumeration = "enum" identifier "{" identifier_list "}" ";".
auto eop_enumeration(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto, auto const& name, auto, auto const& values, auto, auto, auto)
        {
            return Eop{Enumeration{name, values}};
        },
        str("enum"),
        separator,
        token(eop_identifier),
        symbol('{'),
        token(eop_identifier_list),
        symbol('}'),
        whitespace,
        symbol(';')
    )(input);
}

// identifier_list = identifier {"," identifier}.
auto eop_identifier_list(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& identifier, auto const& identifiers)
        {
            return Eop{Identifier_list{identifier, identifiers}};
        },
        eop_identifier,
        repeat(chain(token(symbol(',')), [](auto){ return eop_identifier; }))
    )(input);
}

/*
Structures

A structure is a type consisting of a heterogeneous tuple of named, typed
objects called data members. Each data member is either an individual
object or an array of constant size. In addition, the structure may include
definitions of constructors, a destructor, member operators (assignment,
application, and indexing), and local typedefs. A structure with an apply
operator member is known as a function  object. Omitting the structure
body allows a forward declaration.

A constructor taking a constant reference to the type of the structure is a
copy constructor. If a copy constructor is not defined, a member-by-member
copy constructor is generated. A constructor with no arguments is a default
constructor. A member-by-member default constructor is generated only if
no other constructors are defined. If an assignment operator is not defined,
a member-by-member assignment operator is generated. If no destructor is
supplied, a member-by-member destructor is generated. Each identifier in
an initializer list is the identifier of a data member of the structure. If a
constructor contains an initializer list, every data member of the structure
is constructed with a constructor matching(1) the expression list of the ini-
tializer; all these constructions occur before the body of the constructor is
executed.

(1.) The matching mechanism performs overload resolution by exact matching without any
implicit conversions.
*/

// structure = "struct" structure_name [structure_body] ";".
auto eop_structure(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto, auto const& name, auto const& body, auto, auto)
        {
            return Eop{Structure{name, body}};
        },
        str("struct"),
        separator,
        token(eop_structure_name),
        maybe(eop_structure_body),
        whitespace,
        symbol(';')
    )(input);
}

// structure_name = identifier.
auto eop_structure_name(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& identifier)
        {
            return Eop{Structure_name{identifier}};
        },
        eop_identifier
    )(input);
}

// structure_body = "{" {member} "}".
auto eop_structure_body(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto const& members, auto)
        {
            return Eop{Structure_body{members}};
        },
        symbol('{'),
        repeat(token(eop_member)),
        symbol('}')
    )(input);
}

// member = data_member | constructor | destructor | assign | apply | index | typedef.
auto eop_member(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& member)
        {
            return Eop{Member{member}};
        },
        choice(
            eop_typedef,
            eop_index,
            eop_apply,
            eop_assign,
            eop_destructor,
            eop_constructor,
            eop_data_member
        )
    )(input);
}

// data_member = expression identifier ["[" expression "]"] ";".
auto eop_data_member(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& type, auto, auto const& name, auto const& index, auto)
        {
            return Eop{Data_member{type, name, index}};
        },
        eop_expression,
        separator,
        token(eop_identifier),
        maybe(
            sequence(
                [](auto, auto const& expression, auto){ return expression; },
                symbol('['),
                token(eop_expression),
                symbol(']')
            )
        ),
        symbol(';')
    )(input);
}

// constructor = structure_name "(" [parameter_list] ")" [":" initializer_list] body.
auto eop_constructor(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& name, auto, auto const& parameters, auto, auto const& initializers, auto const& body)
        {
            return Eop{Constructor{name, parameters, initializers, body}};
        },
        eop_structure_name,
        token(symbol('(')),
        maybe(eop_parameter_list),
        token(symbol(')')),
        maybe(
            sequence(
                [](auto, auto const& initializers){ return initializers; },
                symbol(':'),
                token(eop_initializer_list)
            )
        ),
        eop_body
    )(input);
}

// destructor = "~" structure_name "(" ")" body.
auto eop_destructor(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto const& name, auto, auto, auto const& body)
        {
            return Eop{Destructor{name, body}};
        },
        symbol('~'),
        token(eop_structure_name),
        symbol('('),
        token(symbol(')')),
        eop_body
    )(input);
}

// assign = "void" "operator" "=" "(" parameter ")" body.
auto eop_assign(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto, auto, auto, auto, auto const& parameter, auto, auto const& body)
        {
            return Eop{Assign{parameter, body}};
        },
        str("void"),
        separator,
        token(str("operator")),
        symbol('='),
        token(symbol('(')),
        eop_parameter,
        token(symbol(')')),
        eop_body
    )(input);
}

// apply = expression "operator" "(" ")" "(" [parameter_list] ")" body.
auto eop_apply(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& type, auto, auto, auto, auto, auto, auto const& parameters, auto, auto const& body){
            return Eop{Apply{type, parameters, body}};
        },
        eop_expression,
        separator,
        token(str("operator")),
        symbol('('),
        token(symbol(')')),
        symbol('('),
        maybe(token(eop_parameter_list)),
        token(symbol(')')),
        eop_body
    )(input);
}

// index = expression "operator" "[" "]" "(" parameter ")" body.
auto eop_index(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& type, auto, auto, auto, auto, auto, auto const& index, auto, auto const& body)
        {
            return Eop{Index{type, index, body}};
        },
        eop_expression,
        separator,
        token(str("operator")),
        symbol('['),
        token(symbol(']')),
        symbol('('),
        token(eop_parameter),
        token(symbol(')')),
        eop_body
    )(input);
}

// initializer_list = initializer {"," initializer}.
auto eop_initializer_list(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& initializer, auto const& initializers)
        {
            return Eop{Initializer_list{initializer, initializers}};
        },
        eop_initializer,
        repeat(chain(token(symbol(',')), [](auto){ return eop_initializer; }))
    )(input);
}

// initializer = identifier "(" [expression_list] ")".
auto eop_initializer(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& name, auto, auto const& arguments, auto)
        {
            return Eop{Initializer{name, arguments}};
        },
        eop_identifier,
        token(symbol('(')),
        maybe(token(eop_expression_list)),
        symbol(')')
    )(input);
}

/*
Procedures

A procedure consists of its return type or, when no value is returned, void,
followed by its name and parameter list. The name may be an identifier
or an operator. A parameter expression must yield a type. A procedure
signature without a body allows a forward declaration.

Only the listed operators can be defined. A definition for the operator
!= is generated in terms of ==; definitions for the operators > ,<=, and >=
are generated in terms of <. When a procedure is called, the value of each
argument expression is bound to the corresponding parameter, and the body
of the procedure is executed.
*/

// procedure = (expression | "void") procedure_name "(" [parameter_list] ")" (body | ";").
auto eop_procedure(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& type, auto, auto const& name, auto, auto const& parameters, auto, auto const& body)
        {
            return Eop{Procedure{type, name, parameters, body}};
        },
        choice(
            sequence([](auto const& expression){ return std::optional<Eop>{expression}; }, eop_expression),
            sequence([](auto const&){ return std::optional<Eop>{}; }, str("void"))
        ),
        maybe(separator),
        token(eop_procedure_name),
        symbol('('),
        maybe(token(eop_parameter_list)),
        token(symbol(')')),
        choice(
            sequence([](auto const& body){ return std::optional{body}; }, eop_body),
            sequence([](auto){ return std::optional<Eop>{}; }, symbol(';'))
        )
    )(input);
}

// procedure_name = identifier | operator.
auto eop_procedure_name(string_view input) -> Parsed_t<Eop>
{
    return choice(
        sequence([](auto const& x){ return Eop{Procedure_name{x}}; }, eop_operator),
        sequence([](auto const& x){ return Eop{Procedure_name{x}}; }, eop_identifier)
    )(input);
}

// operator = "operator" ("==" | "<" | "+" | "-" | "*" | "/" | "%").
auto eop_operator(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto const& value)
        {
            return Eop{Operator{value}};
        },
        str("operator"),
        choice(
            sequence([](auto){ return Operator::Type::eq_v; }, str("==")),
            sequence([](auto){ return Operator::Type::lt_v; }, str("<")),
            sequence([](auto){ return Operator::Type::plus_v; }, str("+")),
            sequence([](auto){ return Operator::Type::minus_v; }, str("-")),
            sequence([](auto){ return Operator::Type::mul_v; }, str("*")),
            sequence([](auto){ return Operator::Type::quot_v; }, str("/")),
            sequence([](auto){ return Operator::Type::rem_v; }, str("%"))
        )
    )(input);
}

// parameter_list = parameter {"," parameter}.
auto eop_parameter_list(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& parameter, auto const& parameters)
        {
            return Eop{Parameter_list{parameter, parameters}};
        },
        eop_parameter,
        repeat(chain(token(symbol(',')), [](auto){ return eop_parameter; }))
    )(input);
}

// parameter = expression [identifier].
auto eop_parameter(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& type, auto const& name)
        {
            return Eop{Parameter{type, name}};
        },
        eop_expression,
        maybe(token(eop_identifier))
    )(input);
}

// body = compound.
auto eop_body(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& compound)
        {
            return Eop{Body{compound}};
        },
        eop_compound
    )(input);
}

/*
Statements

Statements make up the body of procedures, constructors, destructors, and
member operators:

A simple statement, which is often a procedure call, is evaluated for its
side effects. An assignment applies the assignment operator for the type
of the object on the left-hand side. The first expression for a construction
is a type expression giving the type to be constructed. A construction
without an initialization applies the default constructor. A construction
with a parenthesized expression list applies the matching constructor. A
construction with an equal sign followed by an expression applies the copy
constructor; the expression must have the same type as the object being
constructed.
The return statement returns control to the caller of the current func-
tion with the value of the expression as the function result. The expression
must evaluate to a value of the return type of the function.
The conditional statement executes the first statement if the value of the
expression is true; if the expression is false and there is an else clause,
the second statement is executed. The expression must evaluate to a Boolean.
The switch statement evaluates the expression and then executes the
first statement following a case label with matching value; subsequent state-
ments are executed to the end of the switch statement or until a break
statement is executed. The expression in a switch statement must evaluate
to an integer or enumeration.
The while statement repeatedly evaluates the expression and executes
the statement as long as the expression is true. The do statement repeatedly
executes the statement and evaluates the expression until the expression is
false. In either case, the expression must evaluate to a Boolean.
The compound statement executes the sequence of statements in order.
The goto statement transfers execution to the statement following the
corresponding label in the current function.
The break statement terminates the execution of the smallest enclosing
switch, while, or do statement; execution continues with the statement
following the terminated statement.
The typedef statement defines an alias for a type.
*/

// statement = [identifier ":"] (simple_statement | assignment | construction | control_statement | typedef).
auto eop_statement(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& label, auto const& value)
        {
            return Eop{Statement{label, value}};
        },
        maybe(
            sequence(
                [](auto const& label, auto){ return label; },
                eop_identifier,
                token(symbol(':'))
            )
        ),
        choice(
            eop_typedef,
            eop_control_statement,
            eop_construction,
            eop_assignment,
            eop_simple_statement
        )
    )(input);
}

// simple_statement = expression ";".
auto eop_simple_statement(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& value, auto, auto)
        {
            return Eop{Simple_statement{value}};
        },
        eop_expression,
        whitespace,
        symbol(';')
    )(input);
}

// assignment = expression "=" expression ";".
auto eop_assignment(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& lvalue, auto, auto const& rvalue, auto, auto)
        {
            return Eop{Assignment{lvalue, rvalue}};
        },
        eop_expression,
        token(symbol('=')),
        eop_expression,
        whitespace,
        symbol(';')
    )(input);
}

// construction = expression identifier [initialization] ";".
auto eop_construction(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& type, auto const& name, auto const& initialization, auto, auto)
        {
            return Eop{Construction{type, name, initialization}};
        },
        eop_expression,
        token(eop_identifier),
        maybe(eop_initialization),
        whitespace,
        symbol(';')
    )(input);
}

// initialization = "(" expression_list ")" | "=" expression.
auto eop_initialization(string_view input) -> Parsed_t<Eop>
{
    return choice(
        sequence(
            [](auto, auto const& expressions, auto){ return Eop{Initialization{expressions, {}}}; },
            symbol('('),
            token(eop_expression_list),
            symbol(')')
        ),
        sequence(
            [](auto, auto, auto const& expression){ return Eop{Initialization{{}, expression}}; },
            symbol('='),
            whitespace,
            eop_expression
        )
    )(input);
}

// control_statement = return | conditional | switch | while | do | compound | break | goto.
auto eop_control_statement(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& control_statement)
        {
            return Eop{Control_statement{control_statement}};
        },
        choice(
            eop_goto,
            eop_break,
            eop_compound,
            eop_do,
            eop_while,
            eop_switch,
            eop_conditional,
            eop_return
        )
    )(input);
}

// return = "return" [expression] ";".
auto eop_return(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto const& value, auto, auto)
        {
            return Eop{Return{value}};
        },
        str("return"),
        maybe(
            sequence(
                [](auto, auto const& expression){ return expression; },
                separator,
                token(eop_expression)
            )
        ),
        whitespace,
        symbol(';')
    )(input);
}

// conditional = "if" "(" expression ")" statement ["else" statement].
auto eop_conditional(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto, auto const& condition, auto, auto const& true_statement, auto const& false_statement)
        {
            return Eop{Conditional{condition, true_statement, false_statement}};
        },
        str("if"),
        token(symbol('(')),
        eop_expression,
        token(symbol(')')),
        eop_statement,
        maybe(
            sequence(
                [](auto, auto const& statement){ return statement; },
                token(str("else")),
                eop_statement
            )
        )
    )(input);
}

// switch = "switch" "(" expression ")" "{" {case} "}".
auto eop_switch(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto, auto const& conditional, auto, auto, auto const& cases, auto)
        {
            return Eop{Switch{conditional, cases}};
        },
        str("switch"),
        token(symbol('(')),
        eop_expression,
        token(symbol(')')),
        symbol('{'),
        repeat(token(eop_case)),
        symbol('}')
    )(input);
}

// case = "case" expression ":" {statement}.
auto eop_case(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto, auto const& condition, auto, auto const& statements)
        {
            return Eop{Case{condition, statements}};
        },
        str("case"),
        separator,
        token(eop_expression),
        symbol(':'),
        repeat(
            sequence(
                [](auto, auto const& statement){ return statement; },
                whitespace,
                eop_statement
            )
        )
    )(input);
}

// while = "while" "(" expression ")" statement.
auto eop_while(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto, auto const& condition, auto, auto const& statement)
        {
            return Eop{While{condition, statement}};
        },
        str("while"),
        token(symbol('(')),
        eop_expression,
        token(symbol(')')),
        eop_statement
    )(input);
}

// do = "do" statement "while" "(" expression ")" ";".
auto eop_do(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto, auto const& statement, auto, auto, auto const& condition, auto, auto, auto)
        {
            return Eop{Do{condition, statement}};
        },
        str("do"),
        separator,
        token(eop_statement),
        token(str("while")),
        symbol('('),
        token(eop_expression),
        symbol(')'),
        whitespace,
        symbol(';')
    )(input);
}

// compound = "{" {statement} "}".
auto eop_compound(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto const& value, auto)
        {
            return Eop{Compound{value}};
        },
        symbol('{'),
        repeat(token(eop_statement)),
        symbol('}')
    )(input);
}

// break = "break" ";".
auto eop_break(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto, auto)
        {
            return Eop{Break{}};
        },
        str("break"),
        whitespace,
        symbol(';')
    )(input);
}

// goto = "goto" identifier ";".
auto eop_goto(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto, auto const& value, auto)
        {
            return Eop{Goto{value}};
        },
        str("goto"),
        separator,
        token(eop_identifier),
        symbol(';')
    )(input);
}

// typedef = "typedef" expression identifier ";".
auto eop_typedef(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto, auto const& type, auto const& name, auto, auto)
        {
            return Eop{Typedef{type, name}};
        },
        str("typedef"),
        separator,
        token(eop_expression),
        eop_identifier,
        whitespace,
        symbol(';')
    )(input);
}

/*
Templates

A template allows a structure or procedure to be parameterized by one or
more types or constants. Template definitions and template names use <and> as delimiters.(2)

When a template_name is used as a primary, the template definition is
used to generate a structure or procedure with template parameters replaced
by corresponding template arguments. These template arguments are either
given explicitly as the delimited expression list in the template_name or, for
procedures, may be deduced from the procedure argument types.

(2.) To disambiguate between the use of < and > as relations or as template name delimiters,
once a structure_name or procedure_name is parsed as part of a template, it becomes a
terminal symbol.

A template structure can be specialized, providing an alternative defini-
tion for the template that is considered when the arguments match before
the unspecialized version of the template structure.
When the template definition includes a constraint, the template ar-
gument types and values must satisfy the Boolean expression following
requires.
*/

// template = template_decl (structure | procedure | specialization).
auto eop_template(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& declaration, auto, auto const& definition)
        {
            return Eop{Template{declaration, definition}};
        },
        eop_template_decl,
        whitespace,
        choice(
            eop_structure,
            eop_procedure,
            eop_specialization
        )
    )(input);
}

// specialization = "struct" structure_name "<" additive_list ">" [structure_body] ";".
auto eop_specialization(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto, auto const& name, auto, auto const& arguments, auto, auto const& body, auto)
        {
            return Eop{Specialization{name, arguments, body}};
        },
        str("struct"),
        separator,
        token(eop_structure_name),
        symbol('<'),
        token(eop_additive_list),
        symbol('>'),
        token(maybe(eop_structure_body)),
        symbol(';')
    )(input);
}

// template_decl = "template" "<" [parameter_list] ">" [constraint].
auto eop_template_decl(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto, auto const& parameters, auto, auto, auto const& constraint)
        {
            return Eop{Template_decl{parameters, constraint}};
        },
        str("template"),
        token(symbol('<')),
        maybe(eop_parameter_list),
        whitespace,
        symbol('>'),
        maybe(
            sequence(
                [](auto, auto const& constraint){ return constraint; },
                whitespace,
                eop_constraint
            )
        )
    )(input);
}

// constraint = "requires" "(" expression ")".
auto eop_constraint(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto, auto, auto const& value, auto, auto)
        {
            return Eop{Constraint{Eop{value}}};
        },
        str("requires"),
        token(symbol('(')),
        maybe(eop_expression),
        whitespace,
        symbol(')')
    )(input);
}

// template_name  = (structure_name | procedure_name) ["<" additive_list ">"].
auto eop_template_name(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& name, auto const& arguments)
        {
            template_names.emplace_back(fmt(name)); // See Templates (2.)
            return Eop{Template_name{name, arguments}};
        },
        choice(
            eop_structure_name,
            eop_procedure_name
        ),
        maybe(
            sequence(
                [](auto, auto const& arguments, auto, auto){ return arguments; },
                token(symbol('<')),
                eop_additive_list,
                whitespace,
                symbol('>')
            )
        )
    )(input);
}

// additive_list = additive {"," additive}.
auto eop_additive_list(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& additive, auto const& additives)
        {
            return Eop{Additive_list{additive, additives}};
        },
        eop_additive,
        repeat(chain(token(symbol(',')), [](auto){ return eop_additive; }))
    )(input);
}

/*
Declarations

Declarations make up the body of a program.
*/

// declaration = enum | structure | procedure | template.
auto eop_declaration(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& value)
        {
            return Eop{Declaration{value}};
        },
        choice(
            eop_enumeration,
            eop_structure,
            eop_procedure,
            eop_template
        )
    )(input);
}

// declaration_list = {declaration eol}.
auto eop_declaration_list(string_view input) -> Parsed_t<Eop>
{
    return sequence(
        [](auto const& value)
        {
            return Eop{Declaration_list{value}};
        },
        repeat(
            sequence(
                [](auto const& x){ return x; },
                token(eop_declaration)
            )
        )
    )(input);
}
