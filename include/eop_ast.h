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

#pragma once

#include <memory>
#include <ranges>
#include <optional>
#include <string>
#include <vector>

class Eop;

constexpr auto fmt_formatter = [](auto const& x){ return fmt(x); };

constexpr auto json_formatter = [](auto const& x){ return json(x); };

template <std::ranges::range R, std::invocable<std::ranges::range_value_t<R>> F>
requires std::same_as<std::invoke_result_t<F, std::ranges::range_value_t<R>>, std::string>
std::string to_csv(R const& rng, std::string const& sep, F formatter)
{
    std::string csv;
    auto pos = std::ranges::cbegin(rng);
    while (pos != std::ranges::cend(rng)) {
        csv += formatter(*pos);
        ++pos;
        if (pos != std::ranges::cend(rng)) csv += sep;
    }
    return csv;
}

template <std::ranges::range R, std::invocable<std::ranges::range_value_t<R>> F>
requires std::same_as<std::invoke_result_t<F, std::ranges::range_value_t<R>>, std::string>
std::string to_csv(std::ranges::range_value_t<R> const& first, R const& rest, std::string const& sep, F formatter)
{
    std::string csv;
    csv += formatter(first);
    for (auto const& value : rest) {
        csv += sep + formatter(value);
    }
    return csv;
}

template <
    std::ranges::range R,
    std::invocable<typename std::ranges::range_value_t<R>::first_type> S,
    std::invocable<typename std::ranges::range_value_t<R>::second_type> F>
requires
    std::same_as<std::invoke_result_t<S, typename std::ranges::range_value_t<R>::first_type>, std::string> &&
    std::same_as<std::invoke_result_t<F, typename std::ranges::range_value_t<R>::second_type>, std::string>
std::string to_csv(R const& rng, S sep, F formatter)
{
    std::string csv;
    auto pos = std::ranges::cbegin(rng);
    while (pos != std::ranges::cend(rng)) {
        csv += formatter(pos->second);
        ++pos;
        if (pos != std::ranges::cend(rng)) csv += sep(pos->first);;
    }
    return csv;
}

template <
    std::ranges::range R,
    std::invocable<typename std::ranges::range_value_t<R>::first_type> S,
    std::invocable<typename std::ranges::range_value_t<R>::second_type> F>
requires
    std::same_as<std::invoke_result_t<S, typename std::ranges::range_value_t<R>::first_type>, std::string> &&
    std::same_as<std::invoke_result_t<F, typename std::ranges::range_value_t<R>::second_type>, std::string>
std::string to_csv(typename std::ranges::range_value_t<R>::second_type const& first, R const& rest, S sep, F formatter)
{
    std::string csv;
    csv += formatter(first); // NOTE: FOrmatter cannot be used here
    for (auto const& value : rest) {
        csv += sep(value.first) + formatter(value.second);
    }
    return csv;
}

std::string json_object(std::string const& str);

std::string json_object(std::string const& name, std::string const& value);

std::string json_member(std::string const& name, std::string const& value);

std::string json_string(std::string const& str);

std::string json_array(std::ranges::range auto const& rng)
{
    return '[' + to_csv(rng, ",", json_formatter) + ']';
}

std::string json_array(auto const& value, std::ranges::range auto const& rng)
{
    return '[' + to_csv(value, rng, ",", json_formatter) + ']';
}

std::string json_array(std::ranges::range auto const& rng, auto formatter)
{
    return '[' + to_csv(rng, ",", formatter) + ']';
}

std::string json_array(auto const& value, std::ranges::range auto const& rng, auto formatter)
{
    std::string csv;
    csv += json_formatter(value);
    for (auto const& val : rng) {
        csv += formatter(val);
    }
    return '[' + csv + ']';
}

std::string json_eop(std::string const& type, std::string const& value);

struct Eval;

class Eop
{
    struct Concept
    {
        virtual ~Concept() = default;

        virtual std::string fmt_() const = 0;

        virtual std::string json_() const = 0;

        virtual Eop prune_(Eval& x) const = 0;
    };

    template <typename T>
    struct Model : Concept
    {
        explicit Model(T obj) : data_(std::move(obj)) {}

        std::string fmt_() const override { return fmt(data_); }

        std::string json_() const override { return json(data_); }

        Eop prune_(Eval& eval) const override { return prune(data_, eval); }

        T data_;
    };

    std::shared_ptr<Concept const> self_;

public:

    template <typename T>
    explicit Eop(T obj) : self_(std::make_shared<Model<T>>(std::move(obj))) {}

    friend std::string fmt(Eop const& x);

    friend std::string json(Eop const& x);

    friend Eop prune(Eop const& x, Eval& eval);
};

std::string fmt(std::same_as<std::optional<Eop>> auto const& x)
{
    if (x) {
        return fmt(*x);
    } else {
        return "";
    }
}

std::string json(std::same_as<std::optional<Eop>> auto const& x)
{
    if (x) {
        return json(*x);
    } else {
        return "null";
    }
}

Eop prune(std::same_as<std::optional<Eop>> auto const& x, Eval& eval)
{
    if (x) {
        return Eop{std::optional{prune(*x, eval)}};
    } else {
        return Eop{std::optional<Eop>{}};
    }
}

struct Identifier
{
    std::string value;
};

std::string fmt(Identifier const& x);

std::string json(Identifier const& x);

Eop prune(Identifier const& x, Eval& eval);

struct Literal
{
    Eop value;
};

std::string fmt(Literal const& x);

std::string json(Literal const& x);

Eop prune(Literal const& x, Eval& eval);

struct Boolean
{
    bool value;
};

std::string fmt(Boolean const& x);

std::string json(Boolean const& x);

Eop prune(Boolean const& x, Eval& eval);

struct Integer
{
    int value;
};

std::string fmt(Integer const& x);

std::string json(Integer const& x);

Eop prune(Integer const& x, Eval& eval);

struct Real
{
    double value;
};

std::string fmt(Real const& x);

std::string json(Real const& x);

Eop prune(Real const& x, Eval& eval);

struct Comment
{
    std::string value;
};

std::string fmt(Comment const& x);

std::string json(Comment const& x);

Eop prune(Comment const& x, Eval& eval);

struct Basic_type
{
    enum class Type { bool_v, int_v, double_v };

    Type value;
};

std::string fmt(Basic_type const& x);

std::string json(Basic_type const& x);

Eop prune(Basic_type const& x, Eval& eval);

struct Expression
{
    Eop conjunction;
    std::vector<Eop> conjunctions;
};

std::string fmt(Expression const& x);

std::string json(Expression const& x);

Eop prune(Expression const& x, Eval& eval);

struct Conjunction
{
    Eop equality;
    std::vector<Eop> equalities;
};

std::string fmt(Conjunction const& x);

std::string json(Conjunction const& x);

Eop prune(Conjunction const& x, Eval& eval);

struct Equality
{
    enum class Type { eq_v, ne_v };

    static constexpr auto separator = [](Equality::Type type) -> std::string
    {
        switch (type) {
            case Type::eq_v:
                return " == ";
            case Type::ne_v:
                return " != ";
        }
    };

    static constexpr auto json_formatter = [](std::pair<Type, Eop> const& x) -> std::string
    {
        switch (x.first) {
            case Type::eq_v:
                return json_object(
                    json_member("operator", json_string("==")) + ',' +
                    json_member("value", json(x.second))
                );
            case Type::ne_v:
                return json_object(
                    json_member("operator", json_string("!=")) + ',' +
                    json_member("value", json(x.second))
                );
        }
    };

    Eop relational;
    std::vector<std::pair<Type, Eop>> relationals;
};

std::string fmt(Equality const& x);

std::string json(Equality const& x);

Eop prune(Equality const& x, Eval& eval);

struct Relational
{
    enum class Type { lt_v, gt_v, le_v, ge_v };

    static constexpr auto separator = [](Relational::Type type) -> std::string
    {
        switch (type) {
            case Type::lt_v:
                return " < ";
            case Type::gt_v:
                return " > ";
            case Type::le_v:
                return " <= ";
            case Type::ge_v:
                return " >= ";
        }
    };

    static constexpr auto json_formatter = [](std::pair<Type, Eop> const& x) -> std::string
    {
        switch (x.first) {
            case Type::lt_v:
                return json_object(
                    json_member("operator", json_string("<")) + ',' +
                    json_member("value", json(x.second))
                );
            case Type::gt_v:
                return json_object(
                    json_member("operator", json_string(">")) + ',' +
                    json_member("value", json(x.second))
                );
            case Type::le_v:
                return json_object(
                    json_member("operator", json_string("<=")) + ',' +
                    json_member("value", json(x.second))
                );
            case Type::ge_v:
                return json_object(
                    json_member("operator", json_string(">=")) + ',' +
                    json_member("value", json(x.second))
                );
        }
    };

    Eop additive;
    std::vector<std::pair<Type, Eop>> additives;
};

std::string fmt(Relational const& x);

std::string json(Relational const& x);

Eop prune(Relational const& x, Eval& eval);

struct Additive
{
    enum class Type { plus_v, minus_v };

    static constexpr auto separator = [](Additive::Type type) -> std::string
    {
        switch (type) {
            case Type::plus_v:
                return " + ";
            case Type::minus_v:
                return " - ";
        }
    };

    static constexpr auto json_formatter = [](std::pair<Type, Eop> const& x) -> std::string
    {
        switch (x.first) {
            case Type::plus_v:
                return json_object(
                    json_member("type", json_string("+")) + ',' +
                    json_member("value", json(x.second))
                );
            case Type::minus_v:
                return json_object(
                    json_member("type", json_string("-")) + ',' +
                    json_member("value", json(x.second))
                );
        }
    };

    Eop multiplicative;
    std::vector<std::pair<Type, Eop>> multiplicatives;
};

std::string fmt(Additive const& x);

std::string json(Additive const& x);

Eop prune(Additive const& x, Eval& eval);

struct Multiplicative
{
    enum class Type { mul_v, quot_v, rem_v };

    static constexpr auto separator = [](Multiplicative::Type type) -> std::string
    {
        switch (type) {
            case Type::mul_v:
                return " * ";
            case Type::quot_v:
                return " / ";
            case Type::rem_v:
                return " % ";
        }
    };

    static constexpr auto json_formatter = [](std::pair<Type, Eop> const& x) -> std::string
    {
        switch (x.first) {
            case Type::mul_v:
                return json_object(
                    json_member("type", json_string("*")) + ',' +
                    json_member("value", json(x.second))
                );
            case Type::quot_v:
                return json_object(
                    json_member("type", json_string("/")) + ',' +
                    json_member("value", json(x.second))
                );
            case Type::rem_v:
                return json_object(
                    json_member("type", json_string("%")) + ',' +
                    json_member("value", json(x.second))
                );
        }
    };

    Eop prefix;
    std::vector<std::pair<Type, Eop>> prefixes;
};

std::string fmt(Multiplicative const& x);

std::string json(Multiplicative const& x);

Eop prune(Multiplicative const& x, Eval& eval);

struct Prefix
{
    enum class Type { neg_v, not_v, const_v, none_v };

    std::pair<Type, Eop> value;
};

std::string fmt(Prefix const& x);

std::string json(Prefix const& x);

Eop prune(Prefix const& x, Eval& eval);

struct Ref {};

std::string fmt(Ref const& x);

std::string json(Ref const& x);

Eop prune(Ref const& x, Eval& eval);

struct Postfix
{
    enum class Type { id_v, exprs_v, index_v, ref_v };

    Eop primary;
    std::vector<std::pair<Type, Eop>> post;

    std::string postfix_json() const
    {
        std::string ret{'['};
        for (auto const& p : post) {
            switch (p.first) {
                case Postfix::Type::id_v:
                    ret += json_object(
                        json_member("type", json_string(".")) + ',' +
                        json_member("value", json(p.second))
                    );
                    break;
                case Postfix::Type::exprs_v:
                    ret += json_object(
                        json_member("type", json_string("()")) + ',' +
                        json_member("value", json(p.second))
                    );
                    break;
                case Postfix::Type::index_v:
                    ret += json_object(
                        json_member("type", json_string("[]")) + ',' +
                        json_member("value", json(p.second))
                    );
                    break;
                case Postfix::Type::ref_v:
                    ret += json_object("type", json_string("&"));
                    break;
            }
        }
        return ret + "]";
    }
};

std::string fmt(Postfix const& x);

std::string json(Postfix const& x);

Eop prune(Postfix const& x, Eval& eval);

struct Typename {};

std::string fmt(Typename const& x);

std::string json(Typename const& x);

Eop prune(Typename const& x, Eval& eval);

struct Primary
{
    Eop value;
    bool is_expression;
};

std::string fmt(Primary const& x);

std::string json(Primary const& x);

Eop prune(Primary const& x, Eval& eval);

struct Expression_list
{
    Eop expression;
    std::vector<Eop> expressions;
};

std::string fmt(Expression_list const& x);

std::string json(Expression_list const& x);

Eop prune(Expression_list const& x, Eval& eval);

struct Enumeration
{
    Eop name;
    Eop values;
};

std::string fmt(Enumeration const& x);

std::string json(Enumeration const& x);

Eop prune(Enumeration const& x, Eval& eval);

struct Identifier_list
{
    Eop identifier;
    std::vector<Eop> identifiers;
};

std::string fmt(Identifier_list const& x);

std::string json(Identifier_list const& x);

Eop prune(Identifier_list const& x, Eval& eval);

struct Structure
{
    Eop name;
    std::optional<Eop> body;
};

std::string fmt(Structure const& x);

std::string json(Structure const& x);

Eop prune(Structure const& x, Eval& eval);

struct Structure_name
{
    Eop value;
};

std::string fmt(Structure_name const& x);

std::string json(Structure_name const& x);

Eop prune(Structure_name const& x, Eval& eval);

struct Structure_body
{
    std::vector<Eop> value;
};

std::string fmt(Structure_body const& x);

std::string json(Structure_body const& x);

Eop prune(Structure_body const& x, Eval& eval);

struct Member
{
    Eop value;
};

std::string fmt(Member const& x);

std::string json(Member const& x);

Eop prune(Member const& x, Eval& eval);

struct Data_member
{
    Eop type;
    Eop name;
    std::optional<Eop> index;
};

std::string fmt(Data_member const& x);

std::string json(Data_member const& x);

Eop prune(Data_member const& x, Eval& eval);

struct Constructor
{
    Eop name;
    std::optional<Eop> parameters;
    std::optional<Eop> initializers;
    Eop body;
};

std::string fmt(Constructor const& x);

std::string json(Constructor const& x);

Eop prune(Constructor const& x, Eval& eval);

struct Destructor
{
    Eop name;
    Eop body;
};

std::string fmt(Destructor const& x);

std::string json(Destructor const& x);

Eop prune(Destructor const& x, Eval& eval);

struct Assign
{
    Eop parameter;
    Eop body;
};

std::string fmt(Assign const& x);

std::string json(Assign const& x);

Eop prune(Assign const& x, Eval& eval);

struct Apply
{
    Eop type;
    std::optional<Eop> parameters;
    Eop body;
};

std::string fmt(Apply const& x);

std::string json(Apply const& x);

Eop prune(Apply const& x, Eval& eval);

struct Index
{
    Eop type;
    Eop parameter;
    Eop body;
};

std::string fmt(Index const& x);

std::string json(Index const& x);

Eop prune(Index const& x, Eval& eval);

struct Initializer_list
{
    Eop initializer;
    std::vector<Eop> initializers;
};

std::string fmt(Initializer_list const& x);

std::string json(Initializer_list const& x);

Eop prune(Initializer_list const& x, Eval& eval);

struct Initializer
{
    Eop name;
    std::optional<Eop> arguments;
};

std::string fmt(Initializer const& x);

std::string json(Initializer const& x);

Eop prune(Initializer const& x, Eval& eval);

struct Typedef
{
    Eop type;
    Eop name;
};

std::string fmt(Typedef const& x);

std::string json(Typedef const& x);

Eop prune(Typedef const& x, Eval& eval);

struct Procedure
{
    std::optional<Eop> type;
    Eop name;
    std::optional<Eop> parameters;
    std::optional<Eop> body;
};

std::string fmt(Procedure const& x);

std::string json(Procedure const& x);

Eop prune(Procedure const& x, Eval& eval);

struct Procedure_name
{
    Eop value;
};

std::string fmt(Procedure_name const& x);

std::string json(Procedure_name const& x);

Eop prune(Procedure_name const& x, Eval& eval);

struct Operator
{
    enum class Type { eq_v, lt_v, plus_v, minus_v, mul_v, quot_v, rem_v };

    Type value;
};

std::string fmt(Operator const& x);

std::string json(Operator const& x);

Eop prune(Operator const& x, Eval& eval);

struct Parameter_list
{
    Eop parameter;
    std::vector<Eop> parameters;
};

std::string fmt(Parameter_list const& x);

std::string json(Parameter_list const& x);

Eop prune(Parameter_list const& x, Eval& eval);

struct Parameter
{
    Eop type;
    std::optional<Eop> name;
};

std::string fmt(Parameter const& x);

std::string json(Parameter const& x);

Eop prune(Parameter const& x, Eval& eval);

struct Body
{
    Eop value;
};

std::string fmt(Body const& x);

std::string json(Body const& x);

Eop prune(Body const& x, Eval& eval);

struct Statement
{
    std::optional<Eop> label;
    Eop value;
};

std::string fmt(Statement const& x);

std::string json(Statement const& x);

Eop prune(Statement const& x, Eval& eval);

struct Simple_statement
{
    Eop value;
};

std::string fmt(Simple_statement const& x);

std::string json(Simple_statement const& x);

Eop prune(Simple_statement const& x, Eval& eval);

struct Assignment
{
    Eop lvalue;
    Eop rvalue;
};

std::string fmt(Assignment const& x);

std::string json(Assignment const& x);

Eop prune(Assignment const& x, Eval& eval);

struct Construction
{
    Eop type;
    Eop name;
    std::optional<Eop> initialization;
};

std::string fmt(Construction const& x);

std::string json(Construction const& x);

Eop prune(Construction const& x, Eval& eval);

struct Control_statement
{
    Eop value;
};

std::string fmt(Control_statement const& x);

std::string json(Control_statement const& x);

Eop prune(Control_statement const& x, Eval& eval);

struct Initialization
{
    std::optional<Eop> expressions;
    std::optional<Eop> expression;
};

std::string fmt(Initialization const& x);

std::string json(Initialization const& x);

Eop prune(Initialization const& x, Eval& eval);

struct Return
{
    std::optional<Eop> value;
};

std::string fmt(Return const& x);

std::string json(Return const& x);

Eop prune(Return const& x, Eval& eval);

struct Conditional
{
    Eop condition;
    Eop true_statement;
    std::optional<Eop> false_statement;
};

std::string fmt(Conditional const& x);

std::string json(Conditional const& x);

Eop prune(Conditional const& x, Eval& eval);

struct Switch
{
    Eop conditional;
    std::vector<Eop> cases;
};

std::string fmt(Switch const& x);

std::string json(Switch const& x);

Eop prune(Switch const& x, Eval& eval);

struct Case
{
    Eop condition;
    std::vector<Eop> statements;
};

std::string fmt(Case const& x);

std::string json(Case const& x);

Eop prune(Case const& x, Eval& eval);

struct While
{
    Eop condition;
    Eop statement;
};

std::string fmt(While const& x);

std::string json(While const& x);

Eop prune(While const& x, Eval& eval);

struct Do
{
    Eop condition;
    Eop statement;
};

std::string fmt(Do const& x);

std::string json(Do const& x);

Eop prune(Do const& x, Eval& eval);

struct Compound
{
    std::vector<Eop> value;
};

std::string fmt(Compound const& x);

std::string json(Compound const& x);

Eop prune(Compound const& x, Eval& eval);

struct Break {};

std::string fmt(Break const&);

std::string json(Break const&);

Eop prune(Break const&, Eval& eval);

struct Goto
{
    Eop value;
};

std::string fmt(Goto const& x);

std::string json(Goto const& x);

Eop prune(Goto const& x, Eval& eval);

struct Template
{
    Eop declaration;
    Eop definition;
};

std::string fmt(Template const& x);

std::string json(Template const& x);

Eop prune(Template const& x, Eval& eval);

struct Specialization
{
    Eop name;
    Eop arguments;
    std::optional<Eop> body;
};

std::string fmt(Specialization const& x);

std::string json(Specialization const& x);

Eop prune(Specialization const& x, Eval& eval);

struct Template_decl
{
    std::optional<Eop> parameters;
    std::optional<Eop> constraint;
};

std::string fmt(Template_decl const& x);

std::string json(Template_decl const& x);

Eop prune(Template_decl const& x, Eval& eval);

struct Constraint
{
    Eop value;
};

std::string fmt(Constraint const& x);

std::string json(Constraint const& x);

Eop prune(Constraint const& x, Eval& eval);

struct Template_name
{
    Eop name;
    std::optional<Eop> arguments;
};

std::string fmt(Template_name const& x);

std::string json(Template_name const& x);

Eop prune(Template_name const& x, Eval& eval);

struct Additive_list
{
    Eop additive;
    std::vector<Eop> additives;
};

std::string fmt(Additive_list const& x);

std::string json(Additive_list const& x);

Eop prune(Additive_list const& x, Eval& eval);

struct Declaration
{
    Eop value;
};

std::string fmt(Declaration const& x);

std::string json(Declaration const& x);

Eop prune(Declaration const& x, Eval& eval);

struct Declaration_list
{
    std::vector<Eop> value;
};

std::string fmt(Declaration_list const& x);

std::string json(Declaration_list const& x);

Eop prune(Declaration_list const& x, Eval& eval);

std::string fmt(std::vector<Eop> const& xs);

std::string json(std::vector<Eop> const& xs);

std::vector<Eop> prune(std::vector<Eop> const& xs, Eval& eval);

struct Eval
{
    std::optional<Identifier> label;
    std::optional<bool> boolean_literal;
    std::optional<int> integer_literal;
    std::optional<double> real_literal;
};
