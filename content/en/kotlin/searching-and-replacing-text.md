---
title:    "Kotlin recipe: Searching and replacing text"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why

Searching and replacing text in a program is a common task for many developers. Whether it's for fixing typos, updating variable names, or making extensive changes, having a reliable method for searching and replacing text can greatly improve efficiency and accuracy in programming.

## How To

To search and replace text in Kotlin, we can use the `replace()` function. This function takes in two parameters: the text to search for and the replacement text. Let's see an example:

```Kotlin
var message = "Hello, world!"
message = message.replace("world", "universe")
println(message)
```

This code will print out "Hello, universe!" as the `replace()` function finds the word "world" and replaces it with "universe" in the string.

We can also use regular expressions with the `replace()` function for more advanced searching and replacing. For example:

```Kotlin
var message = "Apples, bananas, oranges, and grapes"
message = message.replace(Regex("[aeiou]"), "*")
println(message)
```

This code uses a regular expression to replace all vowels with an asterisk, resulting in the output "*ppl*s, b*n*n*s, *r*ng*s, *nd gr*p*s".

## Deep Dive

The `replace()` function is actually an extension function of the `CharSequence` class. This means that it can be used not just on a `String`, but also on other types that implement the `CharSequence` interface, such as `StringBuilder` and `StringBuffer`.

Additionally, the `replace()` function also has an optional third parameter, which specifies the maximum number of replacements to make. This can be useful when we only want to replace a certain number of occurrences instead of all of them.

## See Also

- Kotlin documentation on `replace()`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/replace.html
- Regular expressions in Kotlin: https://kotlinlang.org/docs/regular-expressions.html
- More about the CharSequence interface: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-char-sequence/