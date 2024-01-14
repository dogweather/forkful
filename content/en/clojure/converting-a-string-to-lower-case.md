---
title:                "Clojure recipe: Converting a string to lower case"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting strings to lower case is a fundamental operation in many programming languages, including Clojure. This is useful for tasks such as comparing strings without worrying about case sensitivity and formatting data in a consistent way.

## How To

To convert a string to lower case in Clojure, we can use the `lower-case` function. Let's look at an example:

```Clojure
(lower-case "HELLO WORLD")
```

This code will take the input string "HELLO WORLD" and return "hello world". Pretty simple, right?

Now, let's say we have a list of strings and we want to convert all of them to lower case. We can use the `map` function in conjunction with `lower-case` to achieve this. Here's an example:

```Clojure
(map lower-case ["CAT", "DOG", "BIRD"])
```

The output of this code will be `("cat" "dog" "bird")`. Notice how each string in the list has been converted to lower case.

## Deep Dive

For those interested in the technical details, the `lower-case` function in Clojure uses the Java `String` class's `toLowerCase` method under the hood. This ensures consistent behavior across different platforms and avoids any potential issues with special characters.

Additionally, it's worth noting that the `lower-case` function only works on ASCII characters. This means that converting strings with non-English characters may not produce the desired results. If you need to handle non-ASCII characters, you can use the `clojure.string/lower-case` function, which uses the Java `Locale` class to handle different language-specific cases.

## See Also

Here are some helpful resources for further reading on string manipulation in Clojure:

- Official Clojure documentation for `lower-case`: https://clojuredocs.org/clojure.core/lower-case
- Clojure Cheat Sheet: https://clojure.org/api/cheatsheet
- Clojure String API: https://clojure.org/reference/java_interop#_strings