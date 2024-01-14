---
title:                "Fish Shell recipe: Deleting characters matching a pattern"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why 

Have you ever found yourself wanting to quickly delete certain characters in a file or string, but didn't want to manually delete them one by one? With the Fish Shell, you can easily remove characters that match a specific pattern. This can be especially useful when dealing with large amounts of data or when trying to reformat text.

## How To 

To delete characters matching a specific pattern, you will need to use the `string` command and the `-r` option. Here's an example:

```
Fish Shell -r "pattern" string
```

Let's say you have the following string:

```
Hello,@@ World!@@
```

If you want to remove all the '@@' symbols from the string, you can use the `string` command like this:

```
Fish Shell -r "@@" string
```

The output of this command will be:

```
Hello, World!
```

You can also use the wildcard `*` to match any number of characters. For example, if you want to remove all numbers from a string, you can use the following command:

```
Fish Shell -r "[0-9]*" string
```

The output for a string like `abc123def` would be:

```
abcdef
```

## Deep Dive 

The `string` command in Fish Shell uses regular expressions to match patterns. Regular expressions, also known as regex, are a powerful tool for finding specific patterns in text. They are commonly used in programming languages and text editors to search, replace, and format text.

When using the `string` command with the `-r` option, the pattern specified can include a combination of characters, symbols, and metacharacters. Metacharacters are special characters with a specific meaning in regex. Here are a few examples:

- `*` matches any number of characters. For instance, `a*b` would match `ab`, `aaab`, or `aaaaab`.
- `.` matches any single character. So, `a.c` would match `abc`, `adc`, or `axc`.
- `\d` matches any digit from 0 to 9.
- `\s` matches any whitespace character, such as a space or a tab.
- `[ ]` defines a character set, and `-` can be used to specify a range of characters. For example, `[0-9]` would match any single digit.

For more information about regular expressions, you can check out the [Fish Shell documentation](https://fishshell.com/docs/current/cmds/string.html#string).

## See Also 

- [Fish Shell documentation on regular expressions](https://fishshell.com/docs/current/cmds/string.html#string)
- [A Beginner's Guide to Regular Expressions](https://www.digitalocean.com/community/tutorials/understanding-regular-expressions-beginner-tutorial)
- [Online regex tester](https://regexr.com/)

Happy coding!