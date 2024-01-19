---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case is a basic task that transforms all uppercase letters in a string to lowercase versions. Programmers often do it to normalize input for easier comparison or to maintain a consistent format.

## How to:

In the Fish shell, there's no built-in feature to convert to lower case. However, `string` commands and `tr` commands fill this gap.

### String commands

```fish
set myString "WELCOME TO FISH"
set myString (string lower $myString)
echo $myString
```

Output:

```fish
welcome to fish
```

### The 'tr' command

```fish
echo "HELLO, WORLD!" | tr '[:upper:]' '[:lower:]'
```

Output:

```fish
hello, world!
```

## Deep Dive:

Historically, Fish shell didn't have a built-in string manipulation feature until the introduction of its 2.3.0 version in 2016, which came with the `string` command, a tool for string manipulation that works consistently across machines.

Although the examples we've seen accomplish our goal, there is a notable difference between `tr` and `string lower`. The `tr` command doesn't support Unicode out of the box, whereas `string lower` does, so depending on your use case, one method might suit you better than the other.

## See Also:

For deeper insights into character encoding, check out this enlightening [article](https://www.joelonsoftware.com/2003/10/08/the-absolute-minimum-every-software-developer-absolutely-positively-must-know-about-unicode-and-character-sets-no-excuses/) by Joel Spolsky, and visit the [Fish documentation](https://fishshell.com/docs/current/).