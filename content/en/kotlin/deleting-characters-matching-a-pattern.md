---
title:                "Kotlin recipe: Deleting characters matching a pattern"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

When working on a project, it is not uncommon to come across situations where characters need to be deleted from a string based on a specific pattern. This could be for data cleansing, formatting, or other purposes. Knowing how to efficiently delete characters matching a pattern could save time and effort in such scenarios.

## How To

To delete characters matching a pattern in Kotlin, we can use the built-in `replace()` function. This function takes two parameters - the pattern to match and the replacement string. Within the function, we can use regular expressions to match the desired pattern.

Let's consider a simple example where we have a string containing a mix of alphabets and numbers and we want to delete all numbers from the string. Here's how we can achieve this in Kotlin:

```Kotlin
val input = "abc123def456ghi"
 val result = input.replace("[0-9]".toRegex(), "")
 println(result) // output: abcdefghi
```

In the above code, we first declare our input string containing both letters and numbers. Then, using the `replace()` function with a regular expression pattern of `[0-9]` (matching all numbers), we replace those numbers with an empty space, effectively deleting them from the string. The result is then printed, displaying the updated string with no numbers.

Apart from deleting specific characters, we can also use the `replace()` function to replace them with a specific string. For example, if we want to replace all vowels with a `*` symbol, we can use the following code:

```Kotlin
val input = "Hello World"
val result = input.replace("[aeiou]".toRegex(),"*")
println(result) // output: H*ll* W*rld
```

Here, we specify a pattern of `[aeiou]` (matching all vowels) and replace them with the `*` symbol.

## Deep Dive

The `replace()` function accepts two parameters - the first one is a regular expression pattern, and the second one is the replacement string. The pattern can be a simple string or a more complex regular expression. The replacement string can also contain special symbols or references to capture groups in the pattern.

Additionally, the `replace()` function also has an optional third parameter - `ignoreCase`. By default, this parameter is set to `false`, but if set to `true`, it ignores the case when performing the replacement. This can be useful when dealing with strings where the case does not matter.

## See Also

- [Kotlin String class documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Regular expressions in Kotlin](https://kotlinlang.org/docs/regular-expressions.html)

Deleting characters matching a pattern is a handy skill to have when working with strings in Kotlin. Whether it is for data manipulation or formatting, the `replace()` function allows us to quickly and efficiently delete characters based on a specific pattern. So the next time you come across such a scenario, remember to use this function to make your task easier.