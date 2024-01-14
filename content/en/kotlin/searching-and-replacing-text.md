---
title:    "Kotlin recipe: Searching and replacing text"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

When writing code, it is common to come across situations where you need to replace certain parts of a string or text. This may be due to a change in requirements or errors that need to be corrected. Instead of manually going through each line of code, Kotlin offers a simple and efficient way to search and replace text, making your coding experience smoother and more efficient.

## How To

To begin, you will need to use the `replace()` function provided by the Kotlin standard library. This function takes in two parameters - the text or character(s) you want to replace and the text or character(s) you want to replace it with. Below is a simple example of how to use this function:

```Kotlin
val sentence = "Hello World!"
val newSentence = sentence.replace("World", "Kotlin")
println(newSentence)

// Output: Hello Kotlin!
```

In this example, we have created a string variable `sentence` with the value "Hello World!" and used the `replace()` function to replace the word "World" with "Kotlin". The output of this code would be "Hello Kotlin!".

You can also use this function to replace multiple occurrences of a word or part of a string. Simply add the `Regex` parameter to your code as shown in the following example:

```Kotlin
val sentence = "Hello my name is John. John likes to code in Kotlin."
val newSentence = sentence.replace(Regex("John"), "Tommy")
println(newSentence)

// Output: Hello my name is Tommy. Tommy likes to code in Kotlin.
```

In this code, we have replaced all occurrences of "John" with "Tommy" using the `Regex` parameter. This adds more flexibility to your code, allowing you to replace specific patterns or words.

## Deep Dive

It is important to note that the `replace()` function does not modify the original string, but instead returns a new string with the replaced text. This means that you can either assign the new string to a variable, as shown in the examples above, or use it directly in your code.

If you want to replace text regardless of its case (uppercase or lowercase), you can use `Regex(phrase, setOf(RegexOption.IGNORE_CASE))` as your `Regex` parameter. This will ignore the case sensitivity and replace all occurrences of the given phrase.

It is also worth mentioning the `replaceFirst()` function, which works the same as `replace()` but only replaces the first occurrence of the given text. This can be useful in certain scenarios, such as replacing a specific word in a paragraph without altering similar words.

## See Also

- [Kotlin Standard Library - Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/#replace)
- [Kotlin Regex Reference](https://kotlinlang.org/docs/regex.html)
- [Kotlin String Functions and Operations](https://www.geeksforgeeks.org/kotlin-string-functions-and-operations/)