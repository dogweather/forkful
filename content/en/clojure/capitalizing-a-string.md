---
title:                "Clojure recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to capitalize a string in your Clojure code? Maybe you're working with user input or manipulating data from a database. Whatever the case may be, sometimes we need to capitalize a string in order to display it correctly or match it with other data. In this blog post, we'll explore how to easily capitalize strings in Clojure.

## How To

Capitalizing a string in Clojure is a simple task that can be accomplished using the `clojure.string/capitalize` function. Let's take a look at an example:

```Clojure
(clojure.string/capitalize "hello world")
```

The output of this expression would be `"Hello world"`. As you can see, the first letter of the string has been capitalized while the rest of the string remains unchanged. However, if we want to capitalize all the words in a string, we can use the `clojure.string/capitalize-words` function as shown below:

```Clojure
(clojure.string/capitalize-words "hello world")
```

The output of this expression would be `"Hello World"`. Now, let's see how these functions behave with different types of strings:

```Clojure
(clojure.string/capitalize "123abc")
```

In this case, the output would be `"123abc"`. Since the first character is not a letter, it cannot be capitalized. Similarly, the `clojure.string/capitalize-words` function will not capitalize non-letter characters.

```Clojure
(clojure.string/capitalize "Clojure Is Awesome!")
```

The output of this expression would be `"Clojure is awesome!"`. The `clojure.string/capitalize` function will only capitalize the first character, so be aware of the rest of the string when using it.

## Deep Dive

Behind the scenes, the `clojure.string/capitalize` and `clojure.string/capitalize-words` functions use the `java.lang.Character/toTitleCase` method to capitalize the given string. This method converts the first character of a string to its corresponding titlecase character. For example, the lowercase character "e" would be converted to the uppercase character "E".

It's worth noting that the `toTitleCase` method follows the Unicode standard, so it can handle non-ASCII characters as well. This means that strings in other languages can also be capitalized using these functions. In addition, the `clojure.string/capitalize-words` function uses the `java.text.BreakIterator` class to determine word boundaries, making it more reliable for capitalizing strings with complex characters and punctuation.

## See Also

- [Clojure String Functions Documentation](https://clojuredocs.org/clojure.string)
- [Java Character toTitleCase Method](https://docs.oracle.com/javase/7/docs/api/java/lang/Character.html#toTitleCase(int))
- [Java BreakIterator Class](https://docs.oracle.com/javase/7/docs/api/java/text/BreakIterator.html)