---
title:    "Kotlin recipe: Using regular expressions"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why

Regular expressions are a powerful tool for pattern matching and manipulation in programming languages. They allow developers to search for specific patterns in strings and perform actions based on those patterns. This can save time and effort when dealing with large chunks of text, making regular expressions a valuable skill to have in your programming arsenal.

## How To

Regular expressions in Kotlin are represented by the `Regex` class. To use regular expressions, you will need to import this class at the beginning of your code.

```
import kotlin.text.Regex
```

To create a regular expression pattern, you can either use the `Regex(pattern: String)` constructor or the `toRegex()` extension function on a string.

```
val regex = Regex("hello") // using constructor
val regex = "hello".toRegex() // using extension function 
```

To match a regular expression pattern against a string, you can use the `matches(input: CharSequence)` function on the regex object, passing in the string you want to match against as the input.

```
val regex = Regex("hello")
val result = regex.matches("hello world") // returns true
```

Regular expressions also allow you to specify patterns using special characters and symbols. For example, the `.` symbol matches any single character, while the `*` symbol matches any number of characters (including zero). You can also use ranges, such as `[a-z]` to match any lowercase letter.

```
val regex = Regex("[a-z]*")
val result = regex.matches("hello world") // returns true
```

Regular expressions also support capturing groups, which allow you to extract specific parts of a string that match a pattern. This is done by surrounding the desired pattern with parentheses. The captured group can then be accessed using the `groupValues` property on the `MatchResult` object returned by the `matchEntire(input: CharSequence)` function.

```
val regex = Regex("([a-z]+)\\s([a-z]+)") 
val result = regex.matchEntire("hello world") // returns MatchResult
val firstName = result?.groupValues?.get(1) // returns "hello"
val lastName = result?.groupValues?.get(2) // returns "world"
```

## Deep Dive

In addition to the basic functions mentioned above, regular expressions in Kotlin also offer various advanced features such as look-ahead and look-behind assertions, non-capturing groups, and more. These features give developers even more control and flexibility when manipulating strings using regular expressions.

One thing to keep in mind when using regular expressions in Kotlin is that they are compiled at runtime, so it may impact the performance of your code if you use them frequently in long strings. To avoid this, you can declare your regular expressions as `val` rather than `var`, as they will only be compiled once and then reused.

## See Also

- [Kotlin Regular Expressions Documentation](https://kotlinlang.org/docs/regular-expressions.html)
- [Regex101](https://regex101.com/) - an online tool for testing regular expressions
- [Mastering Regular Expressions](https://www.amazon.com/Mastering-Regular-Expressions-Jeffrey-Friedl/dp/0596528124) - a comprehensive guide to regular expressions in various programming languages.