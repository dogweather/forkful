---
title:                "C recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, also known as regex, are an essential tool for any programmer. They allow for efficient and flexible searching, matching and manipulating of text. Whether you're a beginner or an experienced developer, understanding how to use regular expressions can greatly enhance your coding skills.

## How To

Before diving into any coding examples, it's important to understand the basic syntax of regular expressions. A regex consists of a pattern or sequence of characters, which is then used for matching or replacing strings of text. In C programming, regular expressions are used through the `<regex.h>` library.

Let's take a look at a simple example of using regular expressions to match a phone number. The code snippet below shows how to initialize a regex variable, compile the pattern, and check if it matches a given string.

```
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int status;
    char phone_number[] = "555-123-4567";

    status = regcomp(&regex, "^\\d{3}-\\d{3}-\\d{4}$", 0); //compile the pattern

    if (status == 0) {
        if (regexec(&regex, phone_number, 0, NULL, 0) == 0) { //check if the string matches the pattern
            printf("Valid phone number!");
        } else {
            printf("Invalid phone number!");
        }
    }

    regfree(&regex); //free memory after finishing with regex

    return 0;
}
```

In the code above, we use the `regcomp()` and `regexec()` functions to compile and execute the regex. The `^\\d{3}-\\d{3}-\\d{4}$` pattern will match strings in the format of `xxx-xxx-xxxx`, where `x` represents any digit between 0-9. The `regfree()` function is used to free up any memory used by the regex after it's no longer needed.

Let's take a closer look at the regular expression pattern we used in the code above. The `^` symbol indicates the beginning of the string, while the `$` symbol marks the end of the string. The `\\d` represents any digit and the `{3}` indicates that the previous character should appear exactly three times. The `-` is a special character that matches itself, and is used to separate the three groups of digits.

## Deep Dive

Regular expressions can be much more complex than the example we saw above. Here are some features and concepts that are important to understand when working with regular expressions:

- Character classes: These are defined within square brackets `[ ]` and allow for matching multiple characters. For example, `[a-z]` matches any lowercase letter from a to z.
- Quantifiers: These symbols specify how many times a character or pattern should appear. For example, `*` means zero or more, `+` means one or more, and `?` means zero or one.
- Escaping characters: Certain characters such as `^`, `$`, and `()` have a special meaning in regular expressions. To match them literally, we need to use the backslash `\` character before them. For example, `\\` matches a backslash.
- Options: Regular expressions have options that can be used to narrow down the matches or make them case-insensitive. These options are placed at the end of the regex pattern, between forward slashes `/` and can include `g` (global) or `i` (case-insensitive).

For a more in-depth guide on regular expressions, check out this [tutorial](https://www.guru99.com/regular-expressions.html).

## See Also

- [The Power of Regular Expressions in C](https://www.geeksforgeeks.org/the-power-of-regular-expressions-in-c/) by GeeksforGeeks
- [Regex Cheat Sheet](https://www.rexegg.com/regex-quickstart.html) by RexEgg
- [Regex Tutorial](https://www.regular-expressions.info/tutorial.html) by regular-expressions.info