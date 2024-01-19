---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Pattern matching is identifying and isolating specific characters or strings in a larger text based on a defined pattern. Programmers employ this to clean, manipulate data or just simplify strings.

## How to:

In Lua, `gsub` function is the go-to tool to delete characters matching a pattern. Let's consider you're a coffee lover, seeing 'tea' unnerves you. We can help!

```lua
local mood_killer = 'I absolutely love tea.'
local mood_adjustment = string.gsub(mood_killer, 'tea', '')
print(mood_adjustment)
```
This will output: `'I absolutely love .'`

Here 'tea' is the pattern. `gsub` replaced it with nothing thus deleting 'tea'.

## Deep Dive

Borrowed from Perl, pattern matching arrived in Lua circa late 90s. It's lightweight yet doesn't fully support Regular Expressions.

As an alternative to `gsub`, we have the `match` function. However, it only finds the pattern but won't replace. If you find `gsub` overkill, combine `match` with `remove`.

Lua's pattern matching engine is stack-based and hence can backfire for complex expressions. Contrarily, `gsub` not only matches but also counts matched patterns.

```lua
local mood_killer = 'I absolutely love tea. tea. tea.'
local _, count = string.gsub(mood_killer, 'tea', '')
print(count)
```
The output, quite pleasing to our coffee-lover, '`3`'.

Another side of `gsub` is the ability to work with functions instead of strings. Here's modification only applied when user loves 'tea' more than any other drink.

```lua
local mood_killer = 'I absolutely love tea.'
local new_mood = string.gsub(mood_killer, "(%w+)%s*tea", function(s) return (s=='love') and "hate" or s end)
print(new_mood)
```
Output will be: `'I absolutely hate tea.'`

## See Also

Try online Lua editors ([repl.it](https://replit.com/languages/lua), [tutorialspoint](https://www.tutorialspoint.com/execute_lua_online.php)) for hands-on practice. For deeper knowledge, visit [Lua's guide](https://www.lua.org/pil/20.2.html) on pattern matching or [Wikipedia's write-up](https://en.wikipedia.org/wiki/Lua_(programming_language)#Pattern_matching) on Lua.