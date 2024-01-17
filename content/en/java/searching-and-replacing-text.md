---
title:                "Searching and replacing text"
html_title:           "Java recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text is a common task in programming where specific words or characters within a text string are identified and replaced with new ones. Programmers often do this to make changes to their code more efficient and expedient, saving time and reducing errors in the process. It is also useful for making large-scale changes to text files or documents.

## How to:
To search and replace text in Java, we can use the `replace()` method from the `String` class. Let's say we have a sentence, "I love programming," and we want to change "programming" to "coding." We would use the following code:

```Java
String sentence = "I love programming";
String newSentence = sentence.replace("programming", "coding");
System.out.println(newSentence);
```

The output would be: "I love coding."

We can also use the `replaceAll()` method to replace multiple occurrences of a word or phrase. For example, if we have the sentence, "Java is the best programming language," and we want to replace "programming" with "coding" and "best" with "greatest," we would use the following code:

```Java
String sentence = "Java is the best programming language";
String newSentence = sentence.replaceAll("programming|best", "coding|greatest");
System.out.println(newSentence);
```

The output would be: "Java is the greatest coding language."

## Deep Dive:
In the early days of programming, searching and replacing text was done manually, which was a time-consuming and error-prone process. However, with the advent of text editors and integrated development environments (IDEs), this task has become much easier and quicker to accomplish.

Apart from using the `replace()` and `replaceAll()` methods in Java, there are other ways to search and replace text, such as regex (regular expressions) and third-party libraries like Apache Commons. These provide more advanced and customizable options for searching and replacing text.

The implementation details of searching and replacing text vary depending on the programming language, but the general concept remains the same. The string to be searched is scanned for the target word or phrase, and when a match is found, it is replaced with the specified replacement. This process continues until the entire string has been scanned.

## See Also:
- [Java String class documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Regular Expression in Java](https://www.geeksforgeeks.org/regular-expressions-in-java/)
- [Apache Commons library for text manipulation](https://commons.apache.org/proper/commons-text/)