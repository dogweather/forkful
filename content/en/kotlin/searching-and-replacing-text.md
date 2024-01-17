---
title:                "Searching and replacing text"
html_title:           "Kotlin recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is a common task in programming where specific text in a larger document or code needs to be located and replaced with new text. Programmers use this technique to save time and improve efficiency when making changes to large amounts of code.

## How to:

To search and replace text in Kotlin, we will use the ```replace()``` method. This method takes two parameters: the text to search for and the replacement text. Let's take a look at an example:

```
val sentence = "Hello, world!"
val newSentence = sentence.replace("Hello", "Hi")
print(newSentence)
```

In this example, the original sentence is "Hello, world!" and we use the ```replace()``` method to change "Hello" to "Hi". The output will be "Hi, world!".

You can also use regular expressions in the ```replace()``` method for more advanced text replacement. For example:

```
val sentence = "Hello, 2020!"
val newSentence = sentence.replace(Regex("[0-9]"), "1")
print(newSentence)
```

This will replace all numbers in the sentence with "1" and the output will be "Hello, 1111!".

## Deep Dive

Searching and replacing text has been a common practice in programming since the early days of computing. In the past, this was often done manually or with specialized tools, but with modern programming languages like Kotlin, it can be easily achieved with built-in methods like ```replace()```.

Other alternatives for text replacement in Kotlin include the ```replaceFirst()``` and ```replaceAll()``` methods, which have slightly different functionality. Additionally, there are external libraries and packages that offer more advanced text replacement features.

Implementation details for the ```replace()``` method can vary depending on the programming language. In Kotlin, this method is a member function of the ```String``` class and can be used on any string variable.

## See Also

- [Kotlin Documentation for replace()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/replace.html)
- [Java String replace() Method](https://www.w3schools.com/java/ref_string_replace.asp)
- [Regular Expressions in Kotlin](https://www.baeldung.com/kotlin-regular-expressions)