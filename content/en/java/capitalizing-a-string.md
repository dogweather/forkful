---
title:                "Capitalizing a string"
html_title:           "Java recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string refers to converting the first letter of each word in a string to uppercase while keeping the remaining letters lowercase. Programmers often do this to improve the readability and consistency of their code, especially when working with user inputs or displaying data to users.

## How to:

To capitalize a string in Java, we can use the built-in `toUpperCase()` and `toLowerCase()` methods from the `String` class. Here's an example of a method that takes in a string and capitalizes it:

```Java
public static String capitalizeString(String str) {
    String[] words = str.split(" "); // split the string into an array of words
    StringBuilder result = new StringBuilder();

    for (String word : words) { // loop through each word
        String firstLetter = word.substring(0, 1); // get the first letter
        String remainingLetters = word.substring(1); // get the remaining letters
        result.append(firstLetter.toUpperCase() + remainingLetters.toLowerCase() + " "); // capitalize first letter, lowercase remaining letters, and add a space at the end
    }

    return result.toString().trim(); // return the final capitalized string
}

// Sample input and output:
System.out.println(capitalizeString("tEsT sTrInG")); // Output: Test String 
System.out.println(capitalizeString("hello world")); // Output: Hello World
```

## Deep Dive:

Capitalizing strings has been a widely used practice in programming for a long time, with its origins dating back to the early days of computer programming. It helps make code more consistent and easier to read, which is especially important when working on large projects with multiple developers.

There are also other ways to capitalize a string, such as using regular expressions or importing external libraries. However, the method shown in the example is a simple, efficient, and easy to understand way.

## See Also:

If you want to learn more about string manipulation in Java, here are some helpful resources:
- [Java String documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Using Regular Expressions in Java](https://docs.oracle.com/javase/tutorial/essential/regex/index.html)
- [Apache Commons Lang library](https://commons.apache.org/proper/commons-lang/)