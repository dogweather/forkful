---
title:                "Kotlin recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Why 

Have you ever found yourself needing to remove certain characters from a string? Maybe you have a string with extra spaces or punctuation that you want to get rid of. In Kotlin, there is a convenient way to delete characters that match a specific pattern, saving you time and effort. In this blog post, we will explore how to do this and take a deep dive into the process. 

## How To 

To delete characters matching a pattern in Kotlin, we can use the `replace` method on a string. This method takes in two parameters: the pattern to match and the replacement string. Let's take a look at an example:
 
```
Kotlin val str = "Hello! How are you?!"

val newStr = str.replace(Regex("[!?.]"), "")

println(newStr)
```

The output of this code would be: "Hello How are you". We used the `replace` method to remove all instances of the characters exclamation mark, question mark, and period. We specified these characters as a pattern using the `Regex` class and provided the replacement string as an empty string. This essentially removes any matches of our pattern from the original string. 

Another way to delete matching characters is by using the `filter` method. This method takes in a predicate function that returns true or false depending on whether the character should be included in the final string or not. Let's see an example:
 
```
Kotlin val str = "Kotlin is super fun!"

val newStr = str.filter { it.isLetter() }

println(newStr)
```

The output of this code would be: "Kotlinissuperfun". We used the `filter` method with the `isLetter()` function to only include letters in the final string, effectively removing all other characters. 

Keep in mind that both the `replace` and `filter` methods return a new string with the changes, instead of modifying the original string. 

## Deep Dive 

Behind the scenes, Kotlin uses regular expressions to match patterns and perform the deletion. In our examples, we used the `Regex` class to specify our pattern. Regular expressions, also known as regex, are powerful tools for searching and manipulating strings based on patterns. They are supported in many programming languages, including Kotlin, and have their own syntax and rules. If you want to learn more about regular expressions, there are plenty of online resources and tutorials available. 

## See Also 

- Official Kotlin documentation on strings and regular expressions: https://kotlinlang.org/docs/reference/strings.html 
- FreeCodeCamp's tutorial on regular expressions in Kotlin: https://www.freecodecamp.org/news/kotlin-regular-expressions-a-comprehensive-guide/
- Tutorialspoint's interactive lessons on regular expressions in Kotlin: https://www.tutorialspoint.com/kotlin/kotlin_regular_expressions.htm