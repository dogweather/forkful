---
date: 2024-01-20 17:58:20.125940-07:00
description: "Searching and replacing text means swapping out specific strings in\
  \ a block of text with others. Programmers do this for tasks like fixing errors,\u2026"
lastmod: '2024-02-25T18:49:56.631154-07:00'
model: gpt-4-1106-preview
summary: "Searching and replacing text means swapping out specific strings in a block\
  \ of text with others. Programmers do this for tasks like fixing errors,\u2026"
title: Searching and replacing text
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text means swapping out specific strings in a block of text with others. Programmers do this for tasks like fixing errors, updating information, or formatting data.

## How to:
Lua's `string.gsub` function is your go-to for search and replace. It works like this:

```lua
local text = "The quick brown fox jumps over the lazy dog."
local searchText = "lazy"
local replaceWith = "energetic"

local result = string.gsub(text, searchText, replaceWith)

print(result)
```

Output:

```
The quick brown fox jumps over the energetic dog.
```

To replace ALL occurrences, `gsub` does it by default:

```lua
local text = "Apples are sweet. Apples are juicy."
local result = string.gsub(text, "Apples", "Oranges")

print(result)
```

Output:

```
Oranges are sweet. Oranges are juicy.
```

## Deep Dive
Searching and replacing text is not unique to Lua; it's a common feature in programming languages. Lua's `string.gsub` goes back to its string manipulation roots, offering a straightforward approach to handle patterns and replacements.

Historically, `gsub` (global substitution) is influenced by Unix's `sed` command and Perl's powerful pattern matching capabilities. Lua's patterns, albeit simpler than regular expressions found in other languages, can still handle complex matches with a little creativity.

Alternatives to `string.gsub` include manually iterating through strings and constructing replacements—a more error-prone method. For heavy text processing, dedicated parsing libraries can be used.

Implementation-wise, `gsub` can take a function as a replacement argument allowing programmatic control over the substitution.

```lua
local result = string.gsub(text, "(%a+)", function(word)
  return #word > 4 and word:upper() or word
end)
```

This snippet will uppercase words longer than four characters.

## See Also
- The [Programming in Lua book](https://www.lua.org/pil/), provides in-depth knowledge of Lua's programming concepts.
- For Lua's full string pattern capabilities, check the [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/manual.html#6.4.1).
