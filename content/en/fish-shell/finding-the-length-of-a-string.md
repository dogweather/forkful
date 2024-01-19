---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Knowing your Strings in Fish Shell: Calculating Length

## What & Why?
Finding the length of a string reflects the number of characters a string contains. Programmers calculate string lengths majorly for data validation and manipulation, data structures control, and efficient memory utilization.

## How to:
In Fish shell, use the built-in `string length` command:
```fish
> set str "Hello, World!"
> string length -q -- $str
13
```

You see, with `$str` being "Hello, World!", `string length -q -- $str` returned 13, the total number of characters, which include letters, spaces, punctuation, etc.

## Deep Dive
Historically, the ability to find string length was only possible through loops, counting characters one by_one. With high-level languages, like those Fish shell is written in, built-in methods were introduced.

In Fish accepting multiple `set` variables, an alternate approach is through looping:

```fish
> set str "Hello, World!"
> for char in (string split -- $str)
    > math count++
    > end
> echo $count
```

However, the `string length` command is preferred for brevity and efficiency.

Let's dive a bit deeper into `string length`. Behind the curtain, it calculates not simply code points, but a Unicode string's width. This means `string length` will output the correct length even for strings containing characters with multiple code points.

Take the following example:
```fish
> set str "Hello, 你好!"
> string length -q -- $str
11
```

Using a Chinese character, which in Unicode corresponds to two code points, `string length` still correctly showed the number of characters as 11.

## See Also
For more on the `string` command, consult the [Fish Shell Documentation](https://fishshell.com/docs/current/cmds/string.html). For deeper understanding, read this [Discussion On Strings In Unicode](https://unicode.org/faq/char_combmark.html). For broader understanding of Fish shell scripting, the [Fish Shell Scripting Tutorial](https://fishshell.com/docs/current/tutorial.html) serves as an excellent resource.