---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# An Uncluttered Guide to Getting String Length in Kotlin

## What & Why?

Finding the length of a string means getting the number of characters it contains. That's handy when you want to make sure that inputs meet length requirements, or if you need to iterate over each character in a string.

## How To:

In Kotlin, getting the length of a string is a piece of cake. All you need is the `length` property. Have a look at this code:

```kotlin
val myString = "Hello, World!"
println(myString.length)
```

Running this gives us `13` as the output because the string "Hello, World!" contains 13 characters.

## Deep Dive

Historically, calculating the length of a string varied depending on the language. For example, in C, you'd have to manually iterate over each character until hitting null.

Thankfully, Kotlin makes it easier with the built-in `length` property – a feature it shares with many high-level languages like Python, Ruby, etc.

What about alternatives? Although `length` is the default go-to, you could use a manual approach using Kotlin's `forEach` function like this:

```kotlin
var cnt = 0
myString.forEach { cnt++ }
println(cnt)
```

But keep in mind, there's an extra overhead in using the `forEach` approach than using `length` directly.

Without going too in-depth, implementation-wise, `length` in Kotlin is just a property that returns the size of the underlying `CharArray` the string is based on. 

## See Also

Here are few other places you might want to check out:

- The [official Kotlin documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/length.html) on strings, especially the `length` property.
- This [Kotlin string guide by TutorialsPoint](https://www.tutorialspoint.com/kotlin/kotlin_strings.htm) covers a lot more than just the `length` property.
- For deep-dives into more complex Kotlin topics, you might like this [Kotlin cookbook by Ken Kousen](https://www.oreilly.com/library/view/kotlin-cookbook/9781492046672/).