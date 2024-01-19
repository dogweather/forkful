---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extracting Substrings in Kotlin

## What & Why?

Extracting substrings is about carving out a part of a string. It's key in situations where you need to manipulate or analyze segments of data rather than the whole string.

## How to:

In Kotlin, you extract substrings using the `substring()` method. Here's how:

```Kotlin
val str = "Kotlin is cool"
println(str.substring(7..10)) // "is"
```

So, what did we do here? Put simply, we defined a range (7..10) and Kotlin took care of the rest.

Another trick Kotlin has up its sleeve is the overloaded version of `substring()`. This version uses start and end indices, like so:

```Kotlin
val str = "Kotlin is cool"
println(str.substring(0, 6)) // "Kotlin"
```

This snippet tells Kotlin to start at 0 (the very beginning of our string), and cut out at 6.

## Deep Dive

Back in the day, String manipulations were cumbersome and prone to bugs. But, modern languages like Kotlin have simplified this.

And yes, there are alternatives to `substring()`. There's `substringBefore()`, `substringAfter()`, `substringBeforeLast()`, and `substringAfterLast()`. These take delimiters instead of indices and are handy for specific use cases. Just feed them a delimiter and they'll cut the string for you.

Finally, let's talk about implementation. Under the hood, `substring()` works by creating a new string that references the same char array as the original string but with different start and end indexes. This is an important detail if you're aiming for efficiency, because it means no extra memory allocation or copying of elements is involved. 

## See Also

To dive deeper into substrings in Kotlin, you should:
- Refer to [Kotlin's Standard Library Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/substring.html) to discover more about `substring()`.
- Read up on [Efficient Java String Manipulations](https://javarevisited.blogspot.com/2013/01/how-substring-works-in-java-internals-string.html) (applicable to Kotlin!) for an understanding of how substrings are implemented for efficiency.
- Checkout [GeeksForGeeks](https://www.geeksforgeeks.org/kotlin-string-substring/) Kotlin substring tutorial for more examples and related methods.