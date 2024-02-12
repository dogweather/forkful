---
title:                "Refactoring"
aliases:
- /en/lua/refactoring/
date:                  2024-01-25T02:11:48.246297-07:00
model:                 gpt-4-1106-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/refactoring.md"
---

{{< edit_this_page >}}

## What & Why?
Refactoring is the art of tweaking existing code to improve its structure, readability, and efficiency without altering its external behavior. Programmers do it to make their code more maintainable, reduce complexity, and often as a preliminary step before adding new features or fixing bugs.

## How to:
Let’s take a simple Lua function and refactor it. We begin with a function that calculates the sum of numbers in a list but is written without much thought for efficiency or clarity:

```Lua
function sumList(numbers)
    local result = 0
    for i=1, #numbers do
        for j=1, #numbers do
            if i == j then
                result = result + numbers[i]
            end
        end
    end
    return result
end

print(sumList({1, 2, 3, 4})) -- Outputs: 10
```

Refactor to a more efficient and readable version:
```Lua
function sumListRefactored(numbers)
    local result = 0
    for _, value in ipairs(numbers) do
        result = result + value
    end
    return result
end

print(sumListRefactored({1, 2, 3, 4})) -- Still outputs: 10
```

The refactored version gets rid of the redundant inner loop, using `ipairs` to iterate through the list cleanly.

## Deep Dive
Historically, refactoring comes from the Smalltalk programming community in the late 80s and was popularised by Martin Fowler's book 'Refactoring: Improving the Design of Existing Code'. In Lua, refactoring often involves simplifying complex conditionals, breaking down large functions into smaller ones, and optimizing table use to improve performance.

Refactoring in Lua has its caveats; Lua's dynamic nature and flexible typing can make certain refactors, like renaming variables or changing function signatures, riskier if not done cautiously. Tools for static code analysis (like `luacheck`) can lessen such risks. Alternatives include test-driven development (TDD), where code is continuously refactored as an integral part of the development process, in contrast to a separate refactoring phase.

## See Also
- "Programming in Lua" by Roberto Ierusalimschy for best practices and examples.
- "Refactoring: Improving the Design of Existing Code" by Martin Fowler for principles applicable across languages.
- LuaRocks directory (https://luarocks.org/) for tools and modules aimed at maintaining and refactoring Lua code.
