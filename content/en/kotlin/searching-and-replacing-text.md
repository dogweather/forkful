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

## Why
Do you find yourself constantly manually changing the same text in your code? Perhaps you need to fix a typo or update a variable name. Whatever the case may be, searching and replacing text can save you time and hassle.

## How To
Searching and replacing text using Kotlin is a simple process. First, you need to create a string variable that contains the text you want to search through. Then you can use the `replace()` function to specify the text you want to replace and the new text you want to replace it with. Let's take a look at an example:

```
Kotlin
val message = "Hello, world!"
message.replace("world", "universe")
println(message)
```

This code will output: 
```
Hello, universe!
```

You can also use the `replace()` function on multiple strings at once by separating them with a comma within the parentheses. For example:

```
Kotlin
val text = "Hello, name"
text.replace("Hello", "Hi").replace("name", "John")
println(text)
```

This code will output:
```
Hi, John
```

## Deep Dive
Now that you know the basics of searching and replacing text, let's dig a little deeper. The `replace()` function also has two optional parameters that can be used to specify the index of the first occurrence of the text you want to replace, as well as the number of occurrences you want to replace. Here's an example:

```
Kotlin
val sentence = "the quick brown fox jumps over the lazy dog"
sentence.replace("the", "a", 0, 1)
println(sentence)
```

This code will output:
```
a quick brown fox jumps over the lazy dog
```

In this example, we specified an index of 0 (meaning the beginning of the sentence) and only wanted to replace the first occurrence of "the" with "a". 

You can also use the `replace()` function on strings containing a regular expression. This can be particularly useful when you need to replace multiple variations of a word or phrase. For example:

```
Kotlin
val text = "I love cats, dogs, and birds"
text.replace("\\w+s,$", "animals")
println(text)
```

This code will output:
```
I love animals, animals, and animals
```

In this case, we used a regular expression that replaces any words ending in "s," with "animals," effectively replacing all of the animal names in the original string.

## See Also
- [Kotlin strings](https://kotlinlang.org/docs/basic-syntax.html#strings)
- [Regular expressions in Kotlin](https://kotlinlang.org/docs/regular-expressions.html)

Now you know how to efficiently search and replace text using Kotlin. Say goodbye to manually changing text in your code and save time and frustration with the `replace()` function. Happy coding!