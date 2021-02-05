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

#include <iostream>

#include "eop_ast.h"
#include "eop_parser.h"

std::string fmt(Parsed_t<Eop> const& x)
{
    if (x) {
        return fmt(x->first);
    } else {
        return "Error: No parsed object!";
    }
}

std::string json(Parsed_t<Eop> const& x)
{
    if (x) {
        return json(x->first);
    } else {
        return "null";
    }
}

int main()
{
    std::string help{
        "Enter source code or one of the following commands:\n"
        "   ? : help\n"
        "   q : quit\n"
        "   f : format code\n"
        "   j : json-format AST\n"
        "   p : prune AST"
    };

    std::cout << help;

    std::string input;
    Parsed_t<Eop> result;

    while (true)
    {
        std::cout << "\n\n>> ";
        std::getline(std::cin, input);

        if (input == "?") {
            std::cout << '\n' << help;
        }

        if (input == "q") {
            break;
        }

        if (input == "f") {
            std::cout << '\n' << fmt(result);
            continue;
        }

        if (input == "j")
        {
            std::cout << '\n' << json(result);
            continue;
        }

        if (input == "p")
        {
            if (result) {
                Eval eval;
                result->first = prune(result->first, eval);
                std::cout << "\nPruning successful!";
                continue;
            } else {
                std::cout << "\nError: No AST to parse!";
                continue;
            }
        }

        auto ans = eop_declaration(input);
        if (ans) {
            if (ans->second.empty()) {
                std::cout << "\nParsing successful!";
                result = ans;
            } else {
                std::cout << "\nError: '" << ans->second << "' not parsed!";
            }
        } else {
            std::cout << "\nError: Parsing failed!";
        }
    }

    return 0;
}
