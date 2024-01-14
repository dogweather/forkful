---
title:                "Fish Shell recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why 

Regular expressions are a powerful tool for manipulating text and can save you time and frustration when working with large amounts of data. They allow you to search for patterns within a string and perform operations such as replacing, extracting, and validating data. With regular expressions, you can work more efficiently and effectively in the Fish Shell environment.

## How To

To use regular expressions in Fish Shell, you first need to surround the pattern you want to search for with forward slashes. For example, to find all occurrences of the word "apple" in a string, you would use the regular expression /apple/. Let's see an example of this in action:

```
Fish Shell> string="I love apples in all forms"
Fish Shell> echo $string | grep /apple/
I love apples in all forms
```

As you can see, the grep command is used to search for the regular expression in the given string. It then outputs the results, which in this case is the entire string since it contains the word "apple".

Regular expressions can also be used for more complex operations, such as extracting specific information from a string. Let's say we have a string containing a phone number in the format of (123) 456-7890 and we want to extract just the area code. We can use regular expressions to do this as shown below:

```
Fish Shell> string="(123) 456-7890"
Fish Shell> echo $string | grep -o /([0-9]{3})/
(123)
```

Here, we use the -o flag with grep to output only the matching text, and the {3} specifies that we want three digits to be matched.

## Deep Dive

Regular expressions provide a powerful and flexible way of searching and manipulating text in Fish Shell. They allow you to use wildcards, quantifiers, and other special characters to define complex patterns. You can also use them in combination with other commands like sed, awk, and cut to perform more advanced tasks.

One important aspect to keep in mind when using regular expressions is that they are case sensitive by default. For example, /apple/ will only match strings containing "apple" with the letter "a" in lowercase. To make a search case insensitive, you can use the -i flag with grep.

Additionally, regular expressions can be used with character classes to match a range of characters. For example, /[A-Z]/ would match any uppercase letter, and /[0-9]/ would match any digit.

## See Also

- [Fish Shell documentation on regular expressions](https://fishshell.com/docs/current/cmds/grep.html)
- [Regular expressions tutorial](https://www.regular-expressions.info/tutorial.html)
- [An interactive regular expressions tutorial](https://regexone.com/)