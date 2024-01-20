---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern is a common text manipulation task in programming. It helps you massage your data into the format you need, making it easier for your code to process.

## How to:

In Fish Shell, you can use the `string match` command to find patterns and the `string replace` command to delete them. Here is an example:

```fish
# Let's say we have a variable with some text
set text 'This is a pattern and a pattern to delete'

# Use string replace to delete the word 'pattern'
set modified_text (string replace -r 'pattern' '' -- $text)

# Print the modified text
echo $modified_text
```
Sample Output:
```
This is a  and a  to delete 
```

The `-r` option makes 'pattern' act as a regular expression. The `--` is used to specify the source string to process.

## Deep Dive:

The `string match` and `string replace` commands in Fish Shell provide a powerful way for programmers to manipulate text. They are based on regular expressions, a concept that originated in the 1950s and has been adopted by many programming languages.

There are lots of alternatives in Fish Shell for pattern matching. For example, you could use the `string match` command to find all matches and then loop over them to make modifications.

However, `string replace` provides a straightforward way to modify all instances at once and is often more efficient. 

Under the hood, `string replace` uses the PCRE2 (Perl Compatible Regular Expressions) library. This provides a rich set of features for pattern matching, including lookaheads, lookbehinds, and other advanced regex features.

## See Also:

For more details, check out these great resources:

- [Fish Shell Documentation: string replace](https://fishshell.com/docs/current/cmds/string-replace.html)
- [Fish Shell Documentation: string match](https://fishshell.com/docs/current/cmds/string-match.html)
- [PCRE2 Documentation](https://www.pcre.org/current/doc/html/)