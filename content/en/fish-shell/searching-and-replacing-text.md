---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text in code is a common operation that lets you find specific strings and swap them out with others. Programmers do it to modify code quickly, correct errors, or update variables throughout a codebase.

## How to?
Bon voyage! You'll explore manipulating text using `string replace` command in Fish Shell.

```Fish Shell
# searching and replacing the first instance of text 'red' to 'blue' in a string
> echo "My dress is red" | string replace -r -f 'red' 'blue'
My dress is blue
# replacing all instances
> echo "Cherry is red. Roses are red." | string replace -r 'red' 'blue'
Cherry is blue. Roses are blue.
```
Nailed it? Now, continue with a multiline string example.

```Fish Shell
# replacing all instances in a multiline string
> echo "Cherry is red.\nRoses are red." | string replace -r 'red' 'blue'
Cherry is blue.
Roses are blue.
```

## Deep Dive
Understanding your tool's past can kick up your expertise a level. `string` became part of Fish in version 2.3. It acts as an all-in-one-text shop bundling operations like search, replace, length count, and more.

But, you're not stuck on the fish rod. Other languages offer text replacements, usually via functions or methods (`str_replace` in PHP, `replace()` in JavaScript, etc.). Some shells also have their "string replace" mechanisms, like `sed` command in Bash.

Now peek under the hood of Fish's `string replace`. It uses the Boost library's `regex_replace` function, performing substitutions based on regex matches. Don't worry if "Boost" and "regex" made you raise an eyebrow. That's another topic, folks.

## See Also
Want more? Check links below.

- Dive into [Fish documentation](http://fishshell.com/docs/current/index.html) on the `string` command.
- Get your hands dirty with [Boost library](https://www.boost.org/doc/libs).
- Remember [regular expressions](https://en.wikipedia.org/wiki/Regular_expression)? Learn'em, they are powerful.
- If Bash is your thing, catch up with `sed` on [GNU docs](http://www.gnu.org/software/sed/manual/sed.html).