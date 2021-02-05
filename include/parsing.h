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

#include <cctype>
#include <concepts>
#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>

// Basic definitions

template <typename T>
using Parsed_t = std::optional<std::pair<T, std::string_view>>;

template <typename P>
concept Parser =
    std::regular_invocable<P, std::string_view> &&
    requires (std::invoke_result_t<P, std::string_view> result) {
        std::same_as<
            decltype(result),
            Parsed_t<typename decltype(result)::value_type::first_type>>;
    };

template <Parser P>
using Parser_result_t = std::invoke_result_t<P, std::string_view>;

template <Parser P>
using Parser_value_t = typename Parser_result_t<P>::value_type::first_type;

template <typename F, typename... Args>
concept Parser_constructor =
    std::regular_invocable<F, Args...> &&
    Parser<std::invoke_result_t<F, Args...>>;

template <typename F, typename... Args>
requires Parser_constructor<F, Args...>
using Parser_constructor_value_t = std::invoke_result_t<F, Args...>;

constexpr auto item = [](std::string_view input) -> Parsed_t<char>
{
    if (input.empty()) {
        return {};
    } else {
        return {{input[0], input.substr(1)}};
    }
};

// Sequencing parsers

template <typename T>
constexpr Parser auto
unit(T const& thing)
{
    return [thing](std::string_view input) -> Parsed_t<T>
    {
        return {{thing, input}};
    };
}

template <Parser P, Parser_constructor<Parser_value_t<P>> F>
constexpr Parser auto
operator&(P parser, F func)
{
    using Parser_t = Parser_constructor_value_t<F, Parser_value_t<P>>;
    return [=](std::string_view input) -> Parser_result_t<Parser_t>
    {
        if (auto const& result = std::invoke(parser, input)) {
            return std::invoke(std::invoke(func, result->first), result->second);
        } else {
            return {};
        }
    };
}

constexpr Parser auto
chain(Parser auto parser, auto... funcs)
{
    if constexpr (std::is_pointer_v<decltype(parser)>) {
        return ([parser](auto input){ return parser(input); } & ... & funcs);
    } else {
        return (parser & ... & funcs);
    }
}

static_assert(
    chain(unit('y'), [](auto){ return item; })("x") == item("x"),
    "unit<T> must be a left identity element for chain"
);
static_assert(
    chain(item, unit<char>)("x") == item("x"),
    "unit<T> must be a right identity element for chain"
);
static_assert(
    chain(chain(item, [](auto){ return unit(item); }), [](auto){ return unit(item); })("xyz") ==
    chain(item, [](auto){ return chain(unit(item), [](auto){ return unit(item); }); })("xyz"),
    "chain must be associative"
);

constexpr auto papply = []<typename F, typename... Args>(F&& f, Args&&... args)
{
    if constexpr (std::invocable<F, Args...>) {
        return std::invoke(std::forward<F>(f), std::forward<Args>(args)...);
    } else {
        return std::bind_front(std::forward<F>(f), std::forward<Args>(args)...);
    }
};

constexpr Parser auto
operator^(Parser auto p, Parser auto q)
{
    return chain(
        p,
        [q](auto const& f)
        {
            return chain(
                q,
                [f](auto const& x){ return unit(papply(f, x)); }
            );
        }
    );
}

template <typename F, Parser... Ps>
requires std::invocable<F, Parser_value_t<Ps>...>
constexpr Parser auto
sequence(F func, Ps... parsers)
{
    return (unit(func) ^ ... ^ parsers);
}

template <typename T, Parser P, std::regular_invocable<T, Parser_value_t<P>> F>
requires std::convertible_to<std::invoke_result_t<F, T, Parser_value_t<P>>, T>
class reduce_many
{
    T init; P parser; F func;

public:

    reduce_many(T const& thing, P const& p, F const& fn)
        : init{thing}, parser{p}, func{fn}
    {}

    auto operator()(std::string_view input) const -> Parsed_t<T>
    {
        return choice(
            chain(
                parser,
                [this](auto const& thing)
                {
                    return reduce_many{std::invoke(func, init, thing), parser, func};
                }
            ),
            unit(init)
        )(input);
    }
};

template <Parser P>
requires std::same_as<Parser_value_t<P>, char>
Parser auto many(P parser)
{
    return reduce_many(
        std::string{},
        parser,
        [](std::string const& st, char ch){ return st + ch; }
    );
}

template <Parser P>
requires std::same_as<Parser_value_t<P>, char>
Parser auto some(P parser)
{
    return chain(
        parser,
        [=](char ch)
        {
            return chain(
                many(parser),
                [=](std::string const& st){ return unit(std::string(1, ch) + st); }
            );
        }
    );
}

// Making choices

template <typename T>
auto empty = [](std::string_view) -> Parsed_t<T>
{
    return {};
};

template <Parser P, Parser Q>
requires std::convertible_to<Parser_value_t<P>, Parser_value_t<Q>>
constexpr Parser auto
operator|(P p, Q q)
{
    return [=](std::string_view input) -> Parser_result_t<Q>
    {
        if (auto const& result = std::invoke(p, input)) {
            return result;
        } else {
            return std::invoke(q, input);
        }
    };
}

constexpr Parser auto
choice(Parser auto parser, Parser auto... parsers)
{
    if constexpr (std::is_pointer_v<decltype(parser)>) {
        return ([parser](auto input){ return parser(input); } | ... | parsers);
    } else {
        return (parser | ... | parsers);
    }
}

static_assert(
    choice(empty<char>, item)("x") == item("x"),
    "empty<T> must be an identity element for choice"
);

template <Parser P, Parser Q>
constexpr Parser auto
pass_empty(P p, Q q)
{
    return [=](std::string_view input) -> Parser_result_t<Q>
    {
        if (auto const& result = std::invoke(p, input)) {
            return {};
        } else {
            return std::invoke(q, input);
        }
    };
}

// Derived primitives

constexpr Parser auto
skip(Parser auto p, Parser auto q)
{
    return choice(chain(p, [q](auto){ return q; }), q);
}

template <typename Pr, Parser P = decltype(item)>
requires std::predicate<Pr, Parser_value_t<P>>
constexpr Parser auto
satisfy(Pr pred, P parser = item)
{
    return chain(
        parser,
        [pred](auto const& th) -> Parser auto
        {
            return [pred, &th](std::string_view input) -> Parsed_t<Parser_value_t<P>>
            {
                if (std::invoke(pred, th)) return {{th, input}}; else return {};
            };
        }
    );
}

template <Parser P>
constexpr Parser auto
maybe(P parser)
{
    return [parser](std::string_view input) -> Parsed_t<std::optional<Parser_value_t<P>>>
    {
        return choice(
            chain(parser, [](auto const& thing){ return unit(std::optional{thing}); }),
            unit(std::optional<Parser_value_t<P>>{})
        )(input);
    };
}

constexpr Parser auto
digit = satisfy([](char x){ return x >= '0' && x <= '9'; });

constexpr Parser auto
letter = satisfy([](char x){ return (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z'); });

constexpr Parser auto
symbol(char x)
{
    return satisfy([x](char y){ return x == y; });
}

class str
{
    std::string_view match;
public:

    explicit str(std::string_view m) : match{m} {}

    auto operator()(std::string_view input) const -> Parsed_t<std::string>
    {
        if (match.empty()) {
            return unit(std::string{})(input);
        } else {
            return chain(
                symbol(match[0]),
                [this](auto){ return str{match.substr(1)}; },
                [this](auto){ return unit(std::string{match}); }
            )(input);
        }
    }
};

//  Handling spacing

inline Parser auto space = satisfy(::isspace);

inline Parser auto whitespace = many(space);

inline Parser auto eol = choice(str{"\r\n"}, str{"\r"}, str{"\n"});

inline Parser auto separator = choice(eol, chain(space, [](auto ch){ return unit(std::string(1, ch)); }));

Parser auto token(Parser auto parser)
{
    return chain(
        skip(whitespace, parser),
        [](auto const& thing){ return skip(whitespace, unit(thing)); }
    );
}
