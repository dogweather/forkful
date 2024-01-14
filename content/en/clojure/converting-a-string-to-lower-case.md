---
title:    "Clojure recipe: Converting a string to lower case"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case may seem like a simple task, but it's an important part of data manipulation and text processing in many programming projects. By converting strings to lower case, you can ensure consistency and improve the accuracy of your text-based operations.

## How To

To convert a string to lower case in Clojure, you can use the `clojure.string/lower-case` function. Let's take a look at some examples:

```Clojure
(clojure.string/lower-case "HELLO") 
```
Output: hello

```Clojure
(clojure.string/lower-case "H@ppy Birthday") 
```
Output: h@ppy birthday

As you can see, the `lower-case` function takes in a string as an argument and returns the same string in lower case. It's that simple!

If you want to convert a string to lower case within a larger code block, you can use the `str` function to concatenate the converted string with the rest of your code. For example:

```Clojure
(str "The capital of Germany is " (clojure.string/lower-case "BERLIN"))
```
Output: The capital of Germany is berlin

## Deep Dive

So how does the `lower-case` function actually work? In Clojure, all strings are represented as sequences of characters. The `lower-case` function simply maps these characters to their lower case equivalents, using the `clojure.string/lower-case-table` function.

The `lower-case-table` function contains a map of character pairs, with the uppercase character as the key and the lowercase character as the value. For example: 

```Clojure
(clojure.string/lower-case-table "A") 
```
Output: \a

This map is used by the `lower-case` function to replace each character in the string with its lower case equivalent. However, since Clojure is an immutable language, the original string remains unchanged and a new string with the converted characters is returned.

## See Also

- `clojure.string/lower-case` documentation: https://clojuredocs.org/clojure.string/lower-case
- `clojure.string/lower-case-table` documentation: https://clojuredocs.org/clojure.string/lower-case-table
- String functions in Clojure: https://clojure.org/reference/strings