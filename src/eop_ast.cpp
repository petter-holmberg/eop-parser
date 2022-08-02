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

#include <optional>
#include <memory>

#include "eop_ast.h"

using std::pair;
using std::string;
using std::string_view;
using std::vector;

string json_object(string const& str)
{
    return '{' + str + '}';
}

string json_object(string const& name, string const& value)
{
    return "{\"" + name + "\":" + value + '}';
}

string json_member(string const& name, string const& value)
{
    return '"' + name + "\":" + value;
}

string json_string(string const& str)
{
    return '"' + str + '"';
}

string json_eop(string const& type, string const& value)
{
    return "{\"type\":\"" + type + "\",\"value\":" + value + "}";
}

string fmt(Eop const& x)
{
    return x.self_->fmt_();
}

string json(Eop const& x)
{
    return x.self_->json_();
}

Eop prune(Eop const& x, Eval& eval)
{
    return x.self_->prune_(eval);
}

string fmt(Identifier const& x)
{
    return x.value;
}

string json(Identifier const& x)
{
    return json_eop("identifier", json_string(fmt(x)));
}

Eop prune(Identifier const& x, Eval&)
{
    return Eop{x};
}

string fmt(Literal const& x)
{
    return fmt(x.value);
}

string json(Literal const& x)
{
    return json_eop("literal", json(x.value));
}

Eop prune(Literal const& x, Eval& eval)
{
    auto node{prune(x.value, eval)};
    if (eval.boolean_literal) {
        return Eop{Boolean{*eval.boolean_literal}};
    } else if (eval.integer_literal) {
        return Eop{Integer{*eval.integer_literal}};
    } else if (eval.real_literal) {
        return Eop{Real{*eval.real_literal}};
    } else {
        return node;
    }
}

string fmt(Boolean const& x)
{
    return (x.value ? "true" : "false");
}

string json(Boolean const& x)
{
    return json_eop("boolean", fmt(x));
}

Eop prune(Boolean const& x, Eval& eval)
{
    eval.boolean_literal = x.value;
    return Eop{x};
}

string fmt(Integer const& x)
{
    return std::to_string(x.value);
}

string json(Integer const& x)
{
    return json_eop("integer", fmt(x));
}

Eop prune(Integer const& x, Eval& eval)
{
    eval.integer_literal = x.value;
    return Eop{x};
}

string fmt(Real const& x)
{
    return std::to_string(x.value);
}

string json(Real const& x)
{
    return json_eop("real", fmt(x));
}

Eop prune(Real const& x, Eval& eval)
{
    eval.real_literal = x.value;
    return Eop{x};
}

string fmt(Comment const& x)
{
    return "// " + x.value + '\n';
}

string json(Comment const& x)
{
    return json_eop("comment", json_string(fmt(x)));
}

Eop prune(Comment const& x, Eval&)
{
    return Eop{x};
}

string fmt(Basic_type const& x)
{
    switch (x.value) {
        case Basic_type::Type::bool_v:
            return "bool";
        case Basic_type::Type::int_v:
            return "int";
        case Basic_type::Type::double_v:
            return "double";
    }
}

string json(Basic_type const& x)
{
    return json_eop("basic_type", json_string(fmt(x)));
}

Eop prune(Basic_type const& x, Eval&)
{
    return Eop{x};
}

string fmt(Expression const& x)
{
    return to_csv(x.conjunction, x.conjunctions, " || ", fmt_formatter);
}

string json(Expression const& x)
{
    return json_eop("expression", json_array(x.conjunction, x.conjunctions));
}

Eop prune(Expression const& x, Eval& eval)
{
    if (x.conjunctions.empty()) {
        return prune(x.conjunction, eval);
    }

    Eop conjunction{prune(x.conjunction, eval)};
    if (eval.boolean_literal && !*eval.boolean_literal) {
        return Eop{Boolean{false}};
    }

    vector<Eop> conjunctions;
    conjunctions.reserve(x.conjunctions.size());

    for (auto const& c : x.conjunctions)
    {
        Eop pc{prune(c, eval)};
        if (eval.boolean_literal && !*eval.boolean_literal) {
            return Eop{Boolean{false}};
        } else {
            conjunctions.emplace_back(pc);
        }
    }

    return Eop{Expression{conjunction, conjunctions}};
}

string fmt(Conjunction const& x)
{
    return to_csv(x.equality, x.equalities, " && ", fmt_formatter);
}

string json(Conjunction const& x)
{
    return json_eop("conjunction", json_array(x.equality, x.equalities));
}

Eop prune(Conjunction const& x, Eval& eval)
{
    if (x.equalities.empty()) {
        return prune(x.equality, eval);
    }

    Eop equality{prune(x.equality, eval)};
    if (eval.boolean_literal && *eval.boolean_literal) {
        return Eop{Boolean{true}};
    }

    vector<Eop> equalities;
    equalities.reserve(x.equalities.size());

    for (auto const& e : x.equalities)
    {
        Eop pe{prune(e, eval)};
        if (eval.boolean_literal && *eval.boolean_literal) {
            return Eop{Boolean{true}};
        } else {
            equalities.emplace_back(pe);
        }
    }

    return Eop{Conjunction{equality, equalities}};
}

string fmt(Equality const& x)
{
    return to_csv(x.relational, x.relationals, Equality::separator, fmt_formatter);
}

string json(Equality const& x)
{
    return json_eop("equality", json_array(x.relational, x.relationals, Equality::json_formatter));
}

Eop prune(Equality const& x, Eval& eval)
{
    Eval prev_eval{eval};
    vector relationals{pair{Equality::Type::eq_v, prune(x.relational, prev_eval)}};

    for (auto const& [op, rel] : x.relationals)
    {
        Eval cur_eval{eval};
        auto relational = prune(rel, cur_eval);
        if (prev_eval.boolean_literal && cur_eval.boolean_literal) {
            switch (op) {
                case Equality::Type::eq_v:
                    cur_eval.boolean_literal = *prev_eval.boolean_literal == *cur_eval.boolean_literal;
                    break;
                case Equality::Type::ne_v:
                    cur_eval.boolean_literal = *prev_eval.boolean_literal != *cur_eval.boolean_literal;
                    break;
            }
            relationals.back().second = Eop{Boolean{*cur_eval.boolean_literal}};
            break;
        } else if (prev_eval.integer_literal && cur_eval.integer_literal) {
            switch (op) {
                case Equality::Type::eq_v:
                    cur_eval.boolean_literal = *prev_eval.integer_literal == *cur_eval.integer_literal;
                    break;
                case Equality::Type::ne_v:
                    cur_eval.boolean_literal = *prev_eval.integer_literal != *cur_eval.integer_literal;
                    break;
            }
            cur_eval.integer_literal.reset();
            relationals.back().second = Eop{Boolean{*cur_eval.boolean_literal}};
            break;
        } else if (prev_eval.integer_literal && cur_eval.real_literal) {
            switch (op) {
                case Equality::Type::eq_v:
                    cur_eval.boolean_literal = *prev_eval.integer_literal == *cur_eval.real_literal;
                    break;
                case Equality::Type::ne_v:
                    cur_eval.boolean_literal = *prev_eval.integer_literal != *cur_eval.real_literal;
                    break;
            }
            cur_eval.real_literal.reset();
            relationals.back().second = Eop{Boolean{*cur_eval.boolean_literal}};
            break;
        } else if (prev_eval.real_literal && cur_eval.integer_literal) {
            switch (op) {
                case Equality::Type::eq_v:
                    cur_eval.boolean_literal = *prev_eval.real_literal == *cur_eval.integer_literal;
                    break;
                case Equality::Type::ne_v:
                    cur_eval.boolean_literal = *prev_eval.real_literal != *cur_eval.integer_literal;
                    break;
            }
            cur_eval.integer_literal.reset();
            relationals.back().second = Eop{Boolean{*cur_eval.boolean_literal}};
            break;
        } else if (prev_eval.real_literal && cur_eval.real_literal) {
            switch (op) {
                case Equality::Type::eq_v:
                    cur_eval.boolean_literal = *prev_eval.real_literal == *cur_eval.real_literal;
                    break;
                case Equality::Type::ne_v:
                    cur_eval.boolean_literal = *prev_eval.real_literal != *cur_eval.real_literal;
                    break;
            }
            cur_eval.real_literal.reset();
            relationals.back().second = Eop{Boolean{*cur_eval.boolean_literal}};
            break;
        } else {
            relationals.emplace_back(op, relational);
        }
        prev_eval = cur_eval;
    }

    auto relational{relationals[0].second};
    relationals.erase(std::begin(relationals));

    if (relationals.empty()) {
        eval = prev_eval;
        return relational;
    } else {
        return Eop{Equality{relational, relationals}};
    }
}

string fmt(Relational const& x)
{
    return to_csv(x.additive, x.additives, Relational::separator, fmt_formatter);
}

string json(Relational const& x)
{
    return json_eop("relational", json_array(x.additive, x.additives, Relational::json_formatter));
}

Eop prune(Relational const& x, Eval& eval)
{
    Eval prev_eval{eval};
    vector additives{pair{Relational::Type::lt_v, prune(x.additive, prev_eval)}};

    for (auto const& [op, add] : x.additives)
    {
        Eval cur_eval{eval};
        auto additive = prune(add, cur_eval);
        if (prev_eval.boolean_literal && cur_eval.boolean_literal) {
            switch (op) {
                case Relational::Type::lt_v:
                    cur_eval.boolean_literal = *prev_eval.boolean_literal < *cur_eval.boolean_literal;
                    break;
                case Relational::Type::gt_v:
                    cur_eval.boolean_literal = *prev_eval.boolean_literal > *cur_eval.boolean_literal;
                    break;
                case Relational::Type::le_v:
                    cur_eval.boolean_literal = *prev_eval.boolean_literal <= *cur_eval.boolean_literal;
                    break;
                case Relational::Type::ge_v:
                    cur_eval.boolean_literal = *prev_eval.boolean_literal >= *cur_eval.boolean_literal;
                    break;
            }
            additives.back().second = Eop{Boolean{*cur_eval.boolean_literal}};
            break;
        } else if (prev_eval.integer_literal && cur_eval.integer_literal) {
            switch (op) {
                case Relational::Type::lt_v:
                    cur_eval.boolean_literal = *prev_eval.integer_literal < *cur_eval.integer_literal;
                    break;
                case Relational::Type::gt_v:
                    cur_eval.boolean_literal = *prev_eval.integer_literal > *cur_eval.integer_literal;
                    break;
                case Relational::Type::le_v:
                    cur_eval.boolean_literal = *prev_eval.integer_literal <= *cur_eval.integer_literal;
                    break;
                case Relational::Type::ge_v:
                    cur_eval.boolean_literal = *prev_eval.integer_literal >= *cur_eval.integer_literal;
                    break;
            }
            cur_eval.integer_literal.reset();
            additives.back().second = Eop{Boolean{*cur_eval.boolean_literal}};
            break;
        } else if (prev_eval.integer_literal && cur_eval.real_literal) {
            switch (op) {
                case Relational::Type::lt_v:
                    cur_eval.boolean_literal = *prev_eval.integer_literal < *cur_eval.real_literal;
                    break;
                case Relational::Type::gt_v:
                    cur_eval.boolean_literal = *prev_eval.integer_literal > *cur_eval.real_literal;
                    break;
                case Relational::Type::le_v:
                    cur_eval.boolean_literal = *prev_eval.integer_literal <= *cur_eval.real_literal;
                    break;
                case Relational::Type::ge_v:
                    cur_eval.boolean_literal = *prev_eval.integer_literal >= *cur_eval.real_literal;
                    break;
            }
            cur_eval.real_literal.reset();
            additives.back().second = Eop{Boolean{*cur_eval.boolean_literal}};
            break;
        } else if (prev_eval.real_literal && cur_eval.integer_literal) {
            switch (op) {
                case Relational::Type::lt_v:
                    cur_eval.boolean_literal = *prev_eval.real_literal < *cur_eval.integer_literal;
                    break;
                case Relational::Type::gt_v:
                    cur_eval.boolean_literal = *prev_eval.real_literal > *cur_eval.integer_literal;
                    break;
                case Relational::Type::le_v:
                    cur_eval.boolean_literal = *prev_eval.real_literal <= *cur_eval.integer_literal;
                    break;
                case Relational::Type::ge_v:
                    cur_eval.boolean_literal = *prev_eval.real_literal >= *cur_eval.integer_literal;
                    break;
            }
            cur_eval.integer_literal.reset();
            additives.back().second = Eop{Boolean{*cur_eval.boolean_literal}};
            break;
        } else if (prev_eval.real_literal && cur_eval.real_literal) {
            switch (op) {
                case Relational::Type::lt_v:
                    cur_eval.boolean_literal = *prev_eval.real_literal < *cur_eval.real_literal;
                    break;
                case Relational::Type::gt_v:
                    cur_eval.boolean_literal = *prev_eval.real_literal > *cur_eval.real_literal;
                    break;
                case Relational::Type::le_v:
                    cur_eval.boolean_literal = *prev_eval.real_literal <= *cur_eval.real_literal;
                    break;
                case Relational::Type::ge_v:
                    cur_eval.boolean_literal = *prev_eval.real_literal >= *cur_eval.real_literal;
                    break;
            }
            cur_eval.real_literal.reset();
            additives.back().second = Eop{Boolean{*cur_eval.boolean_literal}};
            break;
        } else {
            additives.emplace_back(op, additive);
        }
        prev_eval = cur_eval;
    }

    auto additive{additives[0].second};
    additives.erase(std::begin(additives));

    if (additives.empty()) {
        eval = prev_eval;
        return additive;
    } else {
        return Eop{Relational{additive, additives}};
    }
}

string fmt(Additive const& x)
{
    return to_csv(x.multiplicative, x.multiplicatives, Additive::separator, fmt_formatter);
}

string json(Additive const& x)
{
    return json_eop("additive", json_array(x.multiplicative, x.multiplicatives, Additive::json_formatter));
}

Eop prune(Additive const& x, Eval& eval)
{
    Eval prev_eval{eval};
    vector multiplicatives{pair{Additive::Type::plus_v, prune(x.multiplicative, prev_eval)}};

    for (auto const& [op, mul] : x.multiplicatives)
    {
        Eval cur_eval{eval};
        auto multiplicative = prune(mul, cur_eval);
        if (prev_eval.integer_literal && cur_eval.integer_literal) {
            switch (op) {
                case Additive::Type::plus_v:
                    cur_eval.integer_literal = *prev_eval.integer_literal + *cur_eval.integer_literal;
                    break;
                case Additive::Type::minus_v:
                    cur_eval.integer_literal = *prev_eval.integer_literal - *cur_eval.integer_literal;
                    break;
            }
            multiplicatives.back().second = Eop{Integer{*cur_eval.integer_literal}};
        } else if (prev_eval.integer_literal && cur_eval.real_literal) {
            switch (op) {
                case Additive::Type::plus_v:
                    cur_eval.real_literal = *prev_eval.integer_literal + *cur_eval.real_literal;
                    break;
                case Additive::Type::minus_v:
                    cur_eval.real_literal = *prev_eval.integer_literal - *cur_eval.real_literal;
                    break;
            }
            multiplicatives.back().second = Eop{Real{*cur_eval.real_literal}};
        } else if (prev_eval.real_literal && cur_eval.integer_literal) {
            switch (op) {
                case Additive::Type::plus_v:
                    cur_eval.real_literal = *prev_eval.real_literal + *cur_eval.integer_literal;
                    break;
                case Additive::Type::minus_v:
                    cur_eval.real_literal = *prev_eval.real_literal - *cur_eval.integer_literal;
                    break;
            }
            multiplicatives.back().second = Eop{Real{*cur_eval.real_literal}};
            cur_eval.integer_literal.reset();
        } else if (prev_eval.real_literal && cur_eval.real_literal) {
            switch (op) {
                case Additive::Type::plus_v:
                    cur_eval.real_literal = *prev_eval.real_literal + *cur_eval.real_literal;
                    break;
                case Additive::Type::minus_v:
                    cur_eval.real_literal = *prev_eval.real_literal - *cur_eval.real_literal;
                    break;
            }
            multiplicatives.back().second = Eop{Real{*cur_eval.real_literal}};
        } else {
            multiplicatives.emplace_back(pair{op, multiplicative});
        }
        prev_eval = cur_eval;
    }

    auto multiplicative{multiplicatives[0].second};
    multiplicatives.erase(std::begin(multiplicatives));

    if (multiplicatives.empty()) {
        eval = prev_eval;
        return multiplicative;
    } else {
        return Eop{Additive{multiplicative, multiplicatives}};
    }
}

string fmt(Multiplicative const& x)
{
    return to_csv(x.prefix, x.prefixes, Multiplicative::separator, fmt_formatter);
}

string json(Multiplicative const& x)
{
    return json_eop("multiplicative", json_array(x.prefix, x.prefixes, Multiplicative::json_formatter));
}

Eop prune(Multiplicative const& x, Eval& eval)
{
    Eval prev_eval{eval};
    vector prefixes{pair{Multiplicative::Type::mul_v, prune(x.prefix, prev_eval)}};

    for (auto const& [op, pre] : x.prefixes)
    {
        Eval cur_eval{eval};
        auto prefix = prune(pre, cur_eval);

        if (prev_eval.integer_literal && cur_eval.integer_literal) {
            switch (op) {
                case Multiplicative::Type::mul_v:
                    cur_eval.integer_literal = *prev_eval.integer_literal * *cur_eval.integer_literal;
                    prefixes.back().second = Eop{Integer{*cur_eval.integer_literal}};
                    break;
                case Multiplicative::Type::quot_v:
                    if (*cur_eval.integer_literal == 0) {
                        prefixes.emplace_back(pair{op, prefix});
                    } else {
                        prefixes.back().second = Eop{Integer{*cur_eval.integer_literal}};
                    }
                    break;
                case Multiplicative::Type::rem_v:
                    cur_eval.integer_literal = *prev_eval.integer_literal % *cur_eval.integer_literal;
                    prefixes.back().second = Eop{Integer{*cur_eval.integer_literal}};
                    break;
            }
        } else if (prev_eval.integer_literal && cur_eval.real_literal) {
            switch (op) {
                case Multiplicative::Type::mul_v:
                    cur_eval.real_literal = *prev_eval.integer_literal * *cur_eval.real_literal;
                    prefixes.back().second = Eop{Real{*cur_eval.real_literal}};
                    break;
                case Multiplicative::Type::quot_v:
                    cur_eval.real_literal = *prev_eval.integer_literal / *cur_eval.real_literal;
                    prefixes.back().second = Eop{Real{*cur_eval.real_literal}};
                    break;
                case Multiplicative::Type::rem_v:
                    prefixes.emplace_back(pair{op, prefix});
                    break;
            }
        } else if (prev_eval.real_literal && cur_eval.integer_literal) {
            switch (op) {
                case Multiplicative::Type::mul_v:
                    cur_eval.real_literal = *prev_eval.real_literal * *cur_eval.integer_literal;
                    prefixes.back().second = Eop{Real{*cur_eval.real_literal}};
                    cur_eval.integer_literal.reset();
                    break;
                case Multiplicative::Type::quot_v:
                    cur_eval.real_literal = *prev_eval.real_literal / *cur_eval.integer_literal;
                    prefixes.back().second = Eop{Real{*cur_eval.real_literal}};
                    cur_eval.integer_literal.reset();
                    break;
                case Multiplicative::Type::rem_v:
                    prefixes.emplace_back(pair{op, prefix});
                    break;
            }
        } else if (prev_eval.real_literal && cur_eval.real_literal) {
            switch (op) {
                case Multiplicative::Type::mul_v:
                    cur_eval.real_literal = *prev_eval.real_literal * *cur_eval.real_literal;
                    prefixes.back().second = Eop{Real{*cur_eval.real_literal}};
                    break;
                case Multiplicative::Type::quot_v:
                    cur_eval.real_literal = *prev_eval.real_literal / *cur_eval.real_literal;
                    prefixes.back().second = Eop{Real{*cur_eval.real_literal}};
                    break;
                case Multiplicative::Type::rem_v:
                    prefixes.emplace_back(pair{op, prefix});
                    break;
            }
        } else {
            prefixes.emplace_back(pair{op, prefix});
        }
        prev_eval = cur_eval;
    }

    auto prefix{prefixes[0].second};
    prefixes.erase(std::begin(prefixes));

    if (prefixes.empty()) {
        eval = prev_eval;
        return prefix;
    } else {
        return Eop{Multiplicative{prefix, prefixes}};
    }
}

string fmt(Prefix const& x)
{
    switch (x.value.first) {
        case Prefix::Type::neg_v:
            return '-' + fmt(x.value.second);
        case Prefix::Type::not_v:
            return '!' + fmt(x.value.second);
        case Prefix::Type::const_v:
            return "const " + fmt(x.value.second);
        case Prefix::Type::none_v:
            return fmt(x.value.second);
    }
}

string json(Prefix const& x)
{
    switch (x.value.first) {
        case Prefix::Type::neg_v:
            return json_eop(
                "prefix",
                json_object(
                    json_member("type", json_string("-")) + ',' +
                    json_member("value", json(x.value.second))
                )
            );
        case Prefix::Type::not_v:
            return json_eop(
                "prefix",
                json_object(
                    json_member("type", json_string("!")) + ',' +
                    json_member("value", json(x.value.second))
                )
            );
        case Prefix::Type::const_v:
            return json_eop(
                "prefix",
                json_object(
                    json_member("type", json_string("const")) + ',' +
                    json_member("value", json(x.value.second))
                )
            );
        case Prefix::Type::none_v:
            return json_eop(
                "prefix",
                json_object(
                    json_member("type", "null") + ',' +
                    json_member("value", json(x.value.second))
                )
            );
    }
}

Eop prune(Prefix const& x, Eval& eval)
{
    switch (x.value.first) {
        case Prefix::Type::neg_v:
        {
            Eop neg = prune(x.value.second, eval);
            if (eval.integer_literal) {
                return Eop{Integer{-*eval.integer_literal}};
            } else if (eval.real_literal) {
                return Eop{Real{-*eval.real_literal}};
            } else {
                return Eop{Prefix{pair{x.value.first, neg}}};
            }
        }
        case Prefix::Type::not_v:
        {
            Eop neg = prune(x.value.second, eval);
            if (eval.boolean_literal) {
                return Eop{Boolean{!*eval.boolean_literal}};
            } else if (eval.integer_literal) {
                return Eop{Integer{!*eval.integer_literal}};
            } else {
                return Eop{Prefix{pair{x.value.first, neg}}};
            }
        }
        case Prefix::Type::const_v:
            return Eop{Prefix{pair{x.value.first, prune(x.value.second, eval)}}};
        case Prefix::Type::none_v:
            return prune(x.value.second, eval);
    }
}

string fmt(Ref const&)
{
    return "&";
}

string json(Ref const&)
{
    return json_eop("ref", "null");
}

Eop prune(Ref const& x, Eval&)
{
    return Eop{x};
}

string fmt(Postfix const& x)
{
    string ret{fmt(x.primary)};
    for (auto const& post : x.post) {
        switch (post.first) {
            case Postfix::Type::id_v:
                ret += "." + fmt(post.second);
                break;
            case Postfix::Type::exprs_v:
                ret += "(" + fmt(post.second) + ")";
                break;
            case Postfix::Type::index_v:
                ret += '[' + fmt(post.second) + ']';
                break;
            case Postfix::Type::ref_v:
                ret += '&';
                break;
        }
    }
    return ret;
}

string json(Postfix const& x)
{
    return json_eop(
        "postfix",
        json_object(
            json_member("primary", json(x.primary)) + ',' +
            json_member("post", x.postfix_json())
        )
    );
}

Eop prune(Postfix const& x, Eval& eval)
{
    Eval primary_eval{eval};
    Eop primary{prune(x.primary, primary_eval)};

    if (x.post.empty()) {
        eval = primary_eval;
        return primary;
    } else {
        vector<pair<Postfix::Type, Eop>> posts;
        posts.reserve(x.post.size());

        Eval post_eval{eval};
        for (auto const& p : x.post)
        {
            Eop post{prune(p.second, post_eval)};
            posts.emplace_back(pair{p.first, post});
        }

        return Eop{Postfix{primary, posts}};
    }
}

string fmt(Typename const&)
{
    return "typename";
}

string json(Typename const&)
{
    return json_eop("typename", "null");
}

Eop prune(Typename const& x, Eval&)
{
    return Eop{x};
}

string fmt(Primary const& x)
{
    return (x.is_expression ? '(' + fmt(x.value) + ')' : fmt(x.value));
}

string json(Primary const& x)
{
    return json_eop("primary", json(x.value));
}

Eop prune(Primary const& x, Eval& eval)
{
    return prune(x.value, eval);
}

string fmt(Expression_list const& x)
{
    return to_csv(x.expression, x.expressions, ", ", fmt_formatter);
}

string json(Expression_list const& x)
{
    return json_eop("expression_list", json_array(x.expression, x.expressions));
}

Eop prune(Expression_list const& x, Eval& eval)
{
    Eval expression_eval{eval};
    return Eop{Expression_list{prune(x.expression, expression_eval), prune(x.expressions, eval)}};
}

string fmt(Enumeration const& x)
{
    return "enum " + fmt(x.name) + "{" + fmt(x.values) + "};";
}

string json(Enumeration const& x)
{
    return json_eop(
        "enumeration",
        json_object(
            json_member("name", json(x.name)) + ',' +
            json_member("values", json(x.values))
        )
    );
}

Eop prune(Enumeration const& x, Eval& eval)
{
    Eval name_eval{eval};
    Eop name{prune(x.name, name_eval)};

    Eval values_eval{eval};
    Eop values{prune(x.values, values_eval)};

    return Eop{Enumeration{name, values}};
}

string fmt(Identifier_list const& x)
{
    return to_csv(x.identifier, x.identifiers, ", ", fmt_formatter);
}

string json(Identifier_list const& x)
{
    return json_eop("identifier_list", json_array(x.identifier, x.identifiers));
}

Eop prune(Identifier_list const& x, Eval& eval)
{
    Eval identifier_eval{eval};
    return Eop{Identifier_list{prune(x.identifier, identifier_eval), prune(x.identifiers, eval)}};
}

string fmt(Structure const& x)
{
    return "struct " + fmt(x.name) + fmt(x.body) + ';';
}

string json(Structure const& x)
{
    return json_eop(
        "structure",
        json_object(
            json_member("name", json(x.name)) + ',' +
            json_member("body", json(x.body))
        )
    );
}

Eop prune(Structure const& x, Eval& eval)
{
    Eval name_eval{eval};
    Eop name{prune(x.name, name_eval)};

    Eval body_eval{eval};
    std::optional<Eop> body;
    if (x.body) body = prune(*x.body, body_eval);

    return Eop{Structure{name, body}};
}

string fmt(Structure_name const& x)
{
    return fmt(x.value);
}

string json(Structure_name const& x)
{
    return json_eop("structure_name", json(x.value));
}

Eop prune(Structure_name const& x, Eval& eval)
{
    return prune(x.value, eval);
}

string fmt(Structure_body const& x)
{
    return '{' + to_csv(x.value, " ", fmt_formatter) + '}';
}

string json(Structure_body const& x)
{
    return json_eop("structure_body", json_array(x.value));
}

Eop prune(Structure_body const& x, Eval& eval)
{
    return Eop{Structure_body{prune(x.value, eval)}};
}

string fmt(Member const& x)
{
    return fmt(x.value);
}

string json(Member const& x)
{
    return json_eop("member", json(x.value));
}

Eop prune(Member const& x, Eval& eval)
{
    return prune(x.value, eval);
}

string fmt(Data_member const& x)
{
    return fmt(x.type) + ' ' + fmt(x.name) + (x.index ? '[' + fmt(x.index) + ']' : "") + ';';
}

string json(Data_member const& x)
{
    return json_eop(
        "data_member",
        json_object(
            json_member("type", json(x.type)) + ',' +
            json_member("name", json(x.name)) + ',' +
            json_member("index", json(x.index))
        )
    );
}

Eop prune(Data_member const& x, Eval& eval)
{
    Eval type_eval{eval};
    Eop type{prune(x.type, type_eval)};

    Eval name_eval{eval};
    Eop name{prune(x.name, name_eval)};

    Eval index_eval;
    std::optional<Eop> index;
    if (x.index) index = prune(*x.index, index_eval);

    return Eop{Data_member{type, name, index}};
}

string fmt(Constructor const& x)
{
    return
        fmt(x.name) + '(' +
        fmt(x.parameters) + ')' +
        (x.initializers ? ": " + fmt(x.initializers) : "") +
        fmt(x.body);
}

string json(Constructor const& x)
{
    return json_eop(
        "constructor",
        json_object(
            json_member("name", json(x.name)) + ',' +
            json_member("parameters", json(x.parameters)) + ',' +
            json_member("initializers", json(x.initializers)) + ',' +
            json_member("body", json(x.body))
        )
    );
}

Eop prune(Constructor const& x, Eval& eval)
{
    Eval name_eval{eval};
    Eop name{prune(x.name, name_eval)};

    Eval parameters_eval{eval};
    std::optional<Eop> parameters;
    if (x.parameters) parameters = prune(*x.parameters, parameters_eval);

    Eval initializers_eval{eval};
    std::optional<Eop> initializers;
    if (x.initializers) initializers = prune(*x.initializers, initializers_eval);

    Eval body_eval{eval};
    Eop body{prune(x.body, body_eval)};

    return Eop{Constructor{name, parameters, initializers, body}};
}

string fmt(Destructor const& x)
{
    return '~' + fmt(x.name) + "()" + fmt(x.body);
}

string json(Destructor const& x)
{
    return json_eop(
        "destructor",
        json_object(
            json_member("name", json(x.name)) + ',' +
            json_member("body", json(x.body))
        )
    );
}

Eop prune(Destructor const& x, Eval& eval)
{
    Eval name_eval{eval};
    Eop name{prune(x.name, name_eval)};

    Eval body_eval{eval};
    Eop body{prune(x.body, body_eval)};

    return Eop{Destructor{name, body}};
}

string fmt(Assign const& x)
{
    return "void operator=(" + fmt(x.parameter) + ')' + fmt(x.body);
}

string json(Assign const& x)
{
    return json_eop(
        "assign",
        json_object(
            json_member("parameter", json(x.parameter)) + ',' +
            json_member("body", json(x.body))
        )
    );
}

Eop prune(Assign const& x, Eval& eval)
{
    Eval parameter_eval{eval};
    Eop parameter{prune(x.parameter, parameter_eval)};

    Eval body_eval{eval};
    Eop body{prune(x.body, body_eval)};

    return Eop{Assign{parameter, body}};
}

string fmt(Apply const& x)
{
    return fmt(x.type) + " operator()(" + fmt(x.parameters) + ')' + fmt(x.body);
}

string json(Apply const& x)
{
    return json_eop(
        "apply",
        json_object(
            json_member("type", json(x.type)) + ',' +
            json_member("parameters", json(x.parameters)) + ',' +
            json_member("body", json(x.body))
        )
    );
}

Eop prune(Apply const& x, Eval& eval)
{
    Eval type_eval{eval};
    Eop type{prune(x.type, type_eval)};

    Eval parameters_eval{eval};
    std::optional<Eop> parameters;
    if (x.parameters) parameters = prune(*x.parameters, parameters_eval);

    Eval body_eval{eval};
    Eop body{prune(x.body, body_eval)};

    return Eop{Apply{type, parameters, body}};
}

string fmt(Index const& x)
{
    return fmt(x.type) + " operator[](" + fmt(x.parameter) + ')' + fmt(x.body);
}

string json(Index const& x)
{
    return json_eop(
        "index",
        json_object(
            json_member("type", json(x.type)) + ',' +
            json_member("parameter", json(x.parameter)) + ',' +
            json_member("body", json(x.body))
        )
    );
}

Eop prune(Index const& x, Eval& eval)
{
    Eval type_eval{eval};
    Eop type{prune(x.type, type_eval)};

    Eval parameter_eval{eval};
    Eop parameter{prune(x.parameter, parameter_eval)};

    Eval body_eval{eval};
    Eop body{prune(x.body, body_eval)};

    return Eop{Index{type, parameter, body}};
}

string fmt(Initializer_list const& x)
{
    return to_csv(x.initializer, x.initializers, ", ", fmt_formatter);
}

string json(Initializer_list const& x)
{
    return json_eop("initializer_list", json_array(x.initializer, x.initializers));
}

Eop prune(Initializer_list const& x, Eval& eval)
{
    Eval initializer_eval;
    return Eop{Initializer_list{prune(x.initializer, initializer_eval), prune(x.initializers, eval)}};
}

string fmt(Initializer const& x)
{
    return fmt(x.name) + '(' + fmt(x.arguments) + ')';
}

string json(Initializer const& x)
{
    return json_eop(
        "initializer",
        json_object(
            json_member("name", json(x.name)) + ',' +
            json_member("arguments", json(x.arguments))
        )
    );
}

Eop prune(Initializer const& x, Eval& eval)
{
    Eval name_eval{eval};
    Eop name{prune(x.name, name_eval)};

    Eval arguments_eval;
    std::optional<Eop> arguments;
    if (x.arguments) arguments = prune(*x.arguments, arguments_eval);

    return Eop{Initializer{name, arguments}};
}

string fmt(Typedef const& x)
{
    return "typedef " + fmt(x.type) + ' ' + fmt(x.name) + ';';
}

string json(Typedef const& x)
{
    return json_eop(
        "typedef",
        json_object(
            json_member("type", json(x.type)) + ',' +
            json_member("name", json(x.name))
        )
    );
}

Eop prune(Typedef const& x, Eval& eval)
{
    Eval type_eval{eval};
    Eop type{prune(x.type, type_eval)};

    Eval name_eval{eval};
    Eop name{prune(x.name, name_eval)};

    return Eop{Typedef{type, name}};
}

string fmt(Procedure const& x)
{
    return
        (x.type ? fmt(x.type) : "void") + ' ' +
        fmt(x.name) + '(' +
        fmt(x.parameters) + ')' +
        fmt(x.body);
}

string json(Procedure const& x)
{
    return json_eop(
        "procedure",
        json_object(
            json_member("type", (x.type ? json_string(fmt(x.type)) : "null")) + ',' +
            json_member("name", json(x.name)) + ',' +
            json_member("parameters", (x.parameters ? json(x.parameters) : "null")) + ',' +
            json_member("body", (x.body ? json(x.body) : "null"))
        )
    );
}

Eop prune(Procedure const& x, Eval& eval)
{
    Eval type_eval{eval};
    Eval name_eval{eval};
    Eval parameters_eval{eval};
    Eval body_eval{eval};

    return Eop{
        Procedure{
            x.type ? prune(x.type, type_eval) : x.type,
            prune(x.name, name_eval),
            prune(x.parameters, parameters_eval),
            prune(x.body, body_eval)
        }
    };
}

string fmt(Procedure_name const& x)
{
    return fmt(x.value);
}

string json(Procedure_name const& x)
{
    return json_eop("procedure_name", json(x.value));
}

Eop prune(Procedure_name const& x, Eval& eval)
{
    return prune(x.value, eval);
}

string fmt(Operator const& x)
{
    switch (x.value) {
        case Operator::Type::eq_v: return "operator==";
        case Operator::Type::lt_v: return "operator<";
        case Operator::Type::plus_v: return "operator+";
        case Operator::Type::minus_v: return "operator-";
        case Operator::Type::mul_v: return "operator*";
        case Operator::Type::quot_v: return "operator/";
        case Operator::Type::rem_v: return "operator%";
    }
}

string json(Operator const& x)
{
    return json_eop("operator", json_string(fmt(x)));
}

Eop prune(Operator const& x, Eval&)
{
    return Eop{x};
}

string fmt(Parameter_list const& x)
{
    return to_csv(x.parameter, x.parameters, ", ", fmt_formatter);
}

string json(Parameter_list const& x)
{
    return json_eop("parameter_list", json_array(x.parameter, x.parameters));
}

Eop prune(Parameter_list const& x, Eval& eval)
{
    Eval parameter_eval;
    return Eop{Parameter_list{prune(x.parameter, parameter_eval), prune(x.parameters, eval)}};
}

string fmt(Parameter const& x)
{
    return fmt(x.type) + ' ' + fmt(x.name);
}

string json(Parameter const& x)
{
    return json_eop(
        "parameter",
        json_object(
            json_member("type", json(x.type)) + ',' +
            json_member("name", json(x.name))
        )
    );
}

Eop prune(Parameter const& x, Eval& eval)
{
    Eval type_eval{eval};
    Eop type{prune(x.type, type_eval)};

    Eval name_eval{eval};
    std::optional<Eop> name;
    if (x.name) name = prune(*x.name, name_eval);

    return Eop{Parameter{type, name}};
}

string fmt(Body const& x)
{
    return fmt(x.value);
}

string json(Body const& x)
{
    return json_eop("body", json(x.value));
}

Eop prune(Body const& x, Eval& eval)
{
    return prune(x.value, eval);
}

string fmt(Statement const& x)
{
    return (x.label ? fmt(x.label) + ": " : "") + fmt(x.value);
}

string json(Statement const& x)
{
    return json_eop(
        "statement",
        json_object(
            json_member("label", json(x.label)) + ',' +
            json_member("value", json(x.value))
        )
    );
}

Eop prune(Statement const& x, Eval& eval)
{
    Eval label_eval{eval};
    Eop label{prune(x.label, label_eval)};

    if (label_eval.label) {
        return Eop{Statement{label, prune(x.value, eval)}};
    } else {
        return prune(x.value, eval);
    }
}

string fmt(Simple_statement const& x)
{
    return fmt(x.value) + ';';
}

string json(Simple_statement const& x)
{
    return json_eop("simple_statement", json(x.value));
}

Eop prune(Simple_statement const& x, Eval& eval)
{
    return Eop{Simple_statement{prune(x.value, eval)}};
}

string fmt(Assignment const& x)
{
    return fmt(x.lvalue) + " = " + fmt(x.rvalue) + ';';
}

string json(Assignment const& x)
{
    return json_eop(
        "assignment",
        json_object(
            json_member("lvalue", json(x.lvalue)) + ',' +
            json_member("rvalue", json(x.rvalue))
        )
    );
}

Eop prune(Assignment const& x, Eval& eval)
{
    Eval lvalue_eval{eval};
    Eop lvalue{prune(x.lvalue, lvalue_eval)};

    Eval rvalue_eval{eval};
    Eop rvalue{prune(x.rvalue, rvalue_eval)};

    return Eop{Assignment{lvalue, rvalue}};
}

string fmt(Construction const& x)
{
    return fmt(x.type) + ' ' + fmt(x.name) + fmt(x.initialization) + ';';
}

string json(Construction const& x)
{
    return json_eop(
        "construction",
        json_object(
            json_member("type", json(x.type)) + ',' +
            json_member("name", json(x.name)) + ',' +
            json_member("initialization", json(x.initialization))
        )
    );
}

Eop prune(Construction const& x, Eval& eval)
{
    Eval type_eval{eval};
    Eop type{prune(x.type, type_eval)};

    Eval name_eval{eval};
    Eop name{prune(x.name, name_eval)};

    Eval initialization_eval{eval};
    std::optional<Eop> initialization;
    if (x.initialization) initialization = prune(*x.initialization, initialization_eval);

    return Eop{Construction{type, name, initialization}};
}

string fmt(Control_statement const& x)
{
    return fmt(x.value);
}

string json(Control_statement const& x)
{
    return json_eop("control_statement", json(x.value));
}

Eop prune(Control_statement const& x, Eval& eval)
{
    return prune(x.value, eval);
}

string fmt(Initialization const& x)
{
    return x.expressions ? '(' + fmt(x.expressions) + ')' : " = " + fmt(x.expression);
}

string json(Initialization const& x)
{
    return json_eop("initialization", x.expressions ? json(x.expressions) : json(x.expression));
}

Eop prune(Initialization const& x, Eval& eval)
{
    if (x.expressions) {
        return Eop{Initialization{prune(*x.expressions, eval), {}}};
    } else {
        return Eop{Initialization{{}, prune(*x.expression, eval)}};
    }
}

string fmt(Return const& x)
{
    return "return" + (x.value ? ' ' + fmt(x.value) : "") + ';';
}

string json(Return const& x)
{
    return json_eop("return", json(x.value));
}

Eop prune(Return const& x, Eval& eval)
{
    return Eop{Return{prune(x.value, eval)}};
}

string fmt(Conditional const& x)
{
    return
        "if (" + fmt(x.condition) + ") " +
        fmt(x.true_statement) +
        (x.false_statement ? " else " + fmt(x.false_statement) : "");
}

string json(Conditional const& x)
{
    return json_eop(
        "conditional",
        json_object(
            json_member("condition", json(x.condition)) + ',' +
            json_member("true", json(x.true_statement)) + ',' +
            json_member("false", json(x.false_statement))
        )
    );
}

Eop prune(Conditional const& x, Eval& eval)
{
    Eval condition_eval{eval};
    Eop condition{prune(x.condition, condition_eval)};

    if (condition_eval.boolean_literal) {
        if (*condition_eval.boolean_literal) {
            return prune(x.true_statement, eval);
        } else {
            return prune(x.false_statement, eval);
        }
    } else if (condition_eval.integer_literal) {
        if (*condition_eval.integer_literal) {
            return prune(x.true_statement, eval);
        } else {
            return prune(x.false_statement, eval);
        }
    }

    Eval true_eval{eval};
    Eop true_statement{prune(x.true_statement, true_eval)};

    Eval false_eval{eval};
    std::optional<Eop> false_statement;
    if (x.false_statement) false_statement = prune(*x.false_statement, false_eval);

    return Eop{Conditional{condition, true_statement, false_statement}};
}

string fmt(Switch const& x)
{
    return "switch (" + fmt(x.conditional) + ") {" + to_csv(x.cases, " ", fmt_formatter) + '}';

}

string json(Switch const& x)
{
    return json_eop(
        "switch",
        json_object(
            json_member("conditional", json(x.conditional)) + ',' +
            json_member("cases", json_array(x.cases))
        )
    );
}

Eop prune(Switch const& x, Eval& eval)
{
    Eval conditional_eval{eval};
    Eop conditional{prune(x.conditional, conditional_eval)};

    // NOTE: Could further prune conditionals that evaluate to an integer or bool literal here
    vector cases{prune(x.cases, eval)};

    return Eop{Switch{conditional, cases}};
}

string fmt(Case const& x)
{
    return "case " + fmt(x.condition) + ": " + to_csv(x.statements, " ", fmt_formatter);
}

string json(Case const& x)
{
    return json_eop(
        "case",
        json_object(
            json_member("condition", json(x.condition)) + ',' +
            json_member("statement", json_array(x.statements))
        )
    );
}

Eop prune(Case const& x, Eval& eval)
{
    Eval condition_eval{eval};
    Eop condition{prune(x.condition, condition_eval)};

    vector statements{prune(x.statements, eval)};

    return Eop{Case{condition, statements}};
}

string fmt(While const& x)
{
    return "while (" + fmt(x.condition) + ") " + fmt(x.statement);
}

string json(While const& x)
{
    return json_eop(
        "while",
        json_object(
            json_member("condition", json(x.condition)) + ',' +
            json_member("statement", json(x.statement))
        )
    );
}

Eop prune(While const& x, Eval& eval)
{
    Eval condition_eval{eval};
    Eop condition{prune(x.condition, condition_eval)};

    if (condition_eval.boolean_literal) {
        if (!*condition_eval.boolean_literal) {
            return Eop{std::optional<Eop>{}};
        }
    } else if (condition_eval.integer_literal) {
        if (!*condition_eval.integer_literal) {
            return Eop{std::optional<Eop>{}};
        }
    }

    Eval statement_eval{eval};
    return Eop{While{condition, prune(x.statement, statement_eval)}};
}

string fmt(Do const& x)
{
    return "do " + fmt(x.statement) + " while (" + fmt(x.condition) + ");";
}

string json(Do const& x)
{
    return json_eop(
        "while",
        json_object(
            json_member("condition", json(x.condition)) + ',' +
            json_member("statement", json(x.statement))
        )
    );
}

Eop prune(Do const& x, Eval& eval)
{
    Eval condition_eval{eval};
    Eop condition{prune(x.condition, condition_eval)};

    Eval statement_eval{eval};
    return Eop{Do{condition, prune(x.statement, statement_eval)}};
}

string fmt(Compound const& x)
{
    return '{' + to_csv(x.value, " ", fmt_formatter) + '}';
}

string json(Compound const& x)
{
    return json_eop("compound", json_array(x.value));
}

Eop prune(Compound const& x, Eval& eval)
{
    return Eop{Compound{prune(x.value, eval)}};
}

string fmt(Break const&)
{
    return "break;";
}

string json(Break const&)
{
    return json_eop("break", "null");
}

Eop prune(Break const& x, Eval&)
{
    return Eop{x};
}

string fmt(Goto const& x)
{
    return "goto " + fmt(x.value) + ';';
}

string json(Goto const& x)
{
    return json_eop("goto", json(x.value));
}

Eop prune(Goto const& x, Eval&)
{
    return Eop{x};
}

string fmt(Template const& x)
{
    return fmt(x.declaration) + ' ' + fmt(x.definition);
}

string json(Template const& x)
{
    return json_eop(
        "template",
        json_object(
            json_member("declaration", json(x.declaration)) + ',' +
            json_member("definition", json(x.definition))
        )
    );
}

Eop prune(Template const& x, Eval& eval)
{
    Eval declaration_eval{eval};
    Eop declaration{prune(x.declaration, declaration_eval)};

    Eval definition_eval{eval};
    Eop definition{prune(x.definition, definition_eval)};

    return Eop{Template{declaration, definition}};
}

string fmt(Specialization const& x)
{
    return
        "struct " +
        fmt(x.name) + '<' +
        fmt(x.arguments) + '>' +
        (x.body ? fmt(x.body) : "") + ';';
}

string json(Specialization const& x)
{
    return json_eop(
        "specialization",
        json_object(
            json_member("name", json(x.name)) + ',' +
            json_member("arguments", json(x.arguments)) + ',' +
            json_member("body", json(x.body))
        )
    );
}

Eop prune(Specialization const& x, Eval& eval)
{
    Eval name_eval{eval};
    Eop name{prune(x.name, name_eval)};

    Eval arguments_eval{eval};
    Eop arguments{prune(x.arguments, arguments_eval)};

    Eval body_eval{eval};
    std::optional<Eop> body;
    if (x.body) body = prune(*x.body, body_eval);

    return Eop{Specialization{name, arguments, body}};
}

string fmt(Template_decl const& x)
{
    return
        "template <" +
        fmt(x.parameters) + ">" +
        (x.constraint ? ' ' + fmt(x.constraint) : "");
}

string json(Template_decl const& x)
{
    return json_eop(
        "template_decl",
        json_object(
            json_member("parameters", json(x.parameters)) + ',' +
            json_member("constraint", json(x.constraint))
        )
    );
}

Eop prune(Template_decl const& x, Eval& eval)
{
    Eval parameters_eval{eval};
    std::optional<Eop> parameters;
    if (x.parameters) parameters = prune(*x.parameters, parameters_eval);

    Eval constraint_eval{eval};
    std::optional<Eop> constraint;
    if (x.constraint) constraint = prune(*x.constraint, constraint_eval);

    return Eop{Template_decl{parameters, constraint}};
}

string fmt(Constraint const& x)
{
    return "requires(" + fmt(x.value) + ")";
}

string json(Constraint const& x)
{
    return json_eop("constraint", json(x.value));
}

Eop prune(Constraint const& x, Eval& eval)
{
    return Eop{Constraint{prune(x.value, eval)}};
}

string fmt(Template_name const& x)
{
    return fmt(x.name) + (x.arguments ? ('<' + fmt(x.arguments) + '>') : "");
}

string json(Template_name const& x)
{
    return json_eop(
        "template_name",
        json_object(
            json_member("name", json(x.name)) + ',' +
            json_member("arguments", json(x.arguments))
        )
    );
}

Eop prune(Template_name const& x, Eval& eval)
{
    Eval name_eval{eval};
    Eop name{prune(x.name, name_eval)};

    Eval arguments_eval{eval};
    std::optional<Eop> arguments;
    if (x.arguments) arguments = prune(*x.arguments, arguments_eval);

    return Eop{Template_name{name, arguments}};
}

string fmt(Additive_list const& x)
{
    return to_csv(x.additive, x.additives, ", ", fmt_formatter);
}

string json(Additive_list const& x)
{
    return json_eop("additive_list", json_array(x.additive, x.additives));
}

Eop prune(Additive_list const& x, Eval& eval)
{
    Eval additive_eval;
    return Eop{Additive_list{prune(x.additive, additive_eval), prune(x.additives, eval)}};
}

string fmt(Declaration const& x)
{
    return fmt(x.value);
}

string json(Declaration const& x)
{
    return json_eop("declaration", json(x.value));
}

Eop prune(Declaration const& x, Eval& eval)
{
    return Eop{Declaration{prune(x.value, eval)}};
}

string fmt(Declaration_list const& x)
{
    return to_csv(x.value, "\n", fmt_formatter) + '\n';
}

string json(Declaration_list const& x)
{
    return json_eop("declaration_list", json_array(x.value));
}

Eop prune(Declaration_list const& x, Eval& eval)
{
    return Eop{Declaration_list{prune(x.value, eval)}};
}

string fmt(vector<Eop> const& xs)
{
    string ret;

    for (auto const& x : xs)
    {
        ret += fmt(x);
    }

    return ret;
}

string json(vector<Eop> const& xs)
{
    string ret{"["};

    for (auto const& x : xs)
    {
        ret += fmt(x) + ',';
    }
    if (!xs.empty()) {
        ret.resize(ret.size() - 2);
    }

    return ret + "]";
}

vector<Eop> prune(vector<Eop> const& xs, Eval& eval)
{
    vector<Eop> ret;
    ret.reserve(xs.size());

    for (auto const& x : xs)
    {
        Eval member_eval{eval};
        ret.emplace_back(prune(x, member_eval));
    }

    return ret;
}
