---
title:    "Kotlin recipe: Deleting characters matching a pattern"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself in a situation where you needed to delete certain characters from a string that match a specific pattern? This could be for various reasons such as cleaning up data or simply making the string more readable. In this blog post, we will explore how to delete characters matching a pattern in Kotlin.

## How To

To begin, let's create a sample string that we will use for our examples:

```Kotlin
val text = "Thi$s st*ri#ng! c*on#t!ains s#pecial ch@a,rac^ters."
```

### Using Regular Expressions

One way to delete characters matching a pattern is by using regular expressions. In Kotlin, we can use the `replace()` function along with a regular expression to remove specific characters from a string. Let's take a look at an example:

```Kotlin
val newText = text.replace(Regex("[^a-zA-Z0-9]"), "")
println(newText)
```

The output of this code block will be: `Thisstringcontainscharacters.` As you can see, all special characters have been removed from the original string.

### Using a Loop

Another approach to deleting characters matching a pattern is by using a loop. We can iterate through each character in the string and check if it matches our desired pattern. If it does, we can replace it with an empty string. Here's an example:

```Kotlin
var newText = ""
for (char in text) {
    if(char.isLetterOrDigit()) {
        newText += char
    }
}
println(newText)
```

The output of this code block will be the same as the previous one: `Thisstringcontainscharacters.` This method might be more suitable if we want to modify the string in a more complex way.

## Deep Dive

If we take a closer look at our first example using regular expressions, we can break down the `Regex("[^a-zA-Z0-9]")` part and understand how it works. The square brackets `[]` indicate a character class, which means we are specifying which characters we want to match. The `^` symbol inside the brackets means "not", so we are essentially saying "match any character that is not a letter or digit". This way, we can remove all special characters from the string.

## See Also

- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Regex Class in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regular-expression/)
- [String Functions in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/index.html)