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

#include <string>
#include <string_view>
#include <vector>

#include "parsing.h"

Parser auto character = satisfy([](char x){ return x != '\r' && x != '\n'; });

extern std::vector<std::string> template_names;

template <typename T>
auto appended_vector(std::vector<T> x, T const& y) -> std::vector<T>
{
    x.push_back(y);
    return x;
}

template <Parser P>
Parser auto repeat(P parser)
{
    using T = Parser_value_t<P>;
    using Ts = std::vector<T>;
    return reduce_many(
        Ts{},
        parser,
        [](Ts const& ts, T const& t){ return appended_vector(ts, t); }
    );
}

Parser auto op_pair(std::string const& op_str, Parser auto operand, auto result)
{
    return sequence(
        [result](auto, auto const& rel){ return std::pair{result, rel}; },
        token(str(op_str)),
        operand
    );
}

class Eop;

auto eop_identifier(std::string_view input) -> Parsed_t<Eop>;

auto eop_literal(std::string_view input) -> Parsed_t<Eop>;

auto eop_boolean(std::string_view input) -> Parsed_t<Eop>;

auto eop_integer(std::string_view input) -> Parsed_t<Eop>;

auto eop_real(std::string_view input) -> Parsed_t<Eop>;

auto eop_comment(std::string_view input) -> Parsed_t<Eop>;

auto eop_basic_type(std::string_view input) -> Parsed_t<Eop>;

auto eop_expression(std::string_view input) -> Parsed_t<Eop>;

auto eop_conjunction(std::string_view input) -> Parsed_t<Eop>;

auto eop_equality(std::string_view input) -> Parsed_t<Eop>;

auto eop_relational(std::string_view input) -> Parsed_t<Eop>;

auto eop_additive(std::string_view input) -> Parsed_t<Eop>;

auto eop_multiplicative(std::string_view input) -> Parsed_t<Eop>;

auto eop_prefix(std::string_view input) -> Parsed_t<Eop>;

auto eop_postfix(std::string_view input) -> Parsed_t<Eop>;

auto eop_typename(std::string_view input) -> Parsed_t<Eop>;

auto eop_primary(std::string_view input) -> Parsed_t<Eop>;

auto eop_expression_list(std::string_view input) -> Parsed_t<Eop>;

auto eop_enumeration(std::string_view input) -> Parsed_t<Eop>;

auto eop_identifier_list(std::string_view input) -> Parsed_t<Eop>;

auto eop_structure(std::string_view input) -> Parsed_t<Eop>;

auto eop_structure_name(std::string_view input) -> Parsed_t<Eop>;

auto eop_structure_body(std::string_view input) -> Parsed_t<Eop>;

auto eop_member(std::string_view input) -> Parsed_t<Eop>;

auto eop_data_member(std::string_view input) -> Parsed_t<Eop>;

auto eop_constructor(std::string_view input) -> Parsed_t<Eop>;

auto eop_destructor(std::string_view input) -> Parsed_t<Eop>;

auto eop_assign(std::string_view input) -> Parsed_t<Eop>;

auto eop_apply(std::string_view input) -> Parsed_t<Eop>;

auto eop_index(std::string_view input) -> Parsed_t<Eop>;

auto eop_initializer_list(std::string_view input) -> Parsed_t<Eop>;

auto eop_initializer(std::string_view input) -> Parsed_t<Eop>;

auto eop_procedure(std::string_view input) -> Parsed_t<Eop>;

auto eop_procedure_name(std::string_view input) -> Parsed_t<Eop>;

auto eop_operator(std::string_view input) -> Parsed_t<Eop>;

auto eop_parameter_list(std::string_view input) -> Parsed_t<Eop>;

auto eop_parameter(std::string_view input) -> Parsed_t<Eop>;

auto eop_body(std::string_view input) -> Parsed_t<Eop>;

auto eop_statement(std::string_view input) -> Parsed_t<Eop>;

auto eop_simple_statement(std::string_view input) -> Parsed_t<Eop>;

auto eop_assignment(std::string_view input) -> Parsed_t<Eop>;

auto eop_construction(std::string_view input) -> Parsed_t<Eop>;

auto eop_initialization(std::string_view input) -> Parsed_t<Eop>;

auto eop_control_statement(std::string_view input) -> Parsed_t<Eop>;

auto eop_return(std::string_view input) -> Parsed_t<Eop>;

auto eop_conditional(std::string_view input) -> Parsed_t<Eop>;

auto eop_switch(std::string_view input) -> Parsed_t<Eop>;

auto eop_case(std::string_view input) -> Parsed_t<Eop>;

auto eop_while(std::string_view input) -> Parsed_t<Eop>;

auto eop_do(std::string_view input) -> Parsed_t<Eop>;

auto eop_compound(std::string_view input) -> Parsed_t<Eop>;

auto eop_break(std::string_view input) -> Parsed_t<Eop>;

auto eop_goto(std::string_view input) -> Parsed_t<Eop>;

auto eop_typedef(std::string_view input) -> Parsed_t<Eop>;

auto eop_template(std::string_view input) -> Parsed_t<Eop>;

auto eop_specialization(std::string_view input) -> Parsed_t<Eop>;

auto eop_template_decl(std::string_view input) -> Parsed_t<Eop>;

auto eop_constraint(std::string_view input) -> Parsed_t<Eop>;

auto eop_template_name(std::string_view input) -> Parsed_t<Eop>;

auto eop_additive_list(std::string_view input) -> Parsed_t<Eop>;

auto eop_declaration(std::string_view input) -> Parsed_t<Eop>;

auto eop_declaration_list(std::string_view input) -> Parsed_t<Eop>;
