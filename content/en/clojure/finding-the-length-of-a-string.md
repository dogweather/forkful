---
title:    "Clojure recipe: Finding the length of a string"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why
Have you ever wanted to know the length of a string in Clojure? Maybe you need to validate user input or manipulate data in a certain way. In this blog post, we'll explore how to find the length of a string in Clojure, and why it's a useful skill to have in your coding toolbox.

## How To
To find the length of a string in Clojure, we can use the `count` function. Let's look at an example:

```Clojure
(count "Hello World") ; Output: 11
```

In the above code, we use the `count` function to find the length of the string "Hello World". The output is 11, which is the number of characters in the string.

We can also use the `count` function on other data types, such as lists and vectors:

```Clojure
(count [1 2 3 4]) ; Output: 4
(count '(1 2 3 4)) ; Output: 4
```

Notice that the output is the same for both lists and vectors, as they are both considered collections in Clojure.

Another useful function for finding the length of a string is `str-length`. This function specifically counts the number of Unicode characters in a string, rather than just the number of characters. Here's an example:

```Clojure
(str-length "こんにちは") ; Output: 5
```

The Japanese word "こんにちは" contains 5 Unicode characters, but has a length of 15 if we use the `count` function.

## Deep Dive
Now that we know how to find the length of a string in Clojure, let's dive into some deeper information about this topic.

First, it's important to note that the `count` function has a time complexity of O(1), which means it will always have the same execution time regardless of the length of the string. This is because the function simply reads the metadata of the string to retrieve its length, rather than looping through the string itself.

Another interesting point is that spaces and punctuation are also counted as characters in a string. This can be useful to keep in mind if you're trying to manipulate or compare strings in your code.

Lastly, it's worth mentioning that Clojure also has a `empty?` function which can be used to check if a string is empty. This function returns `true` if the string has a length of 0, and `false` if it has a length greater than 0.

## See Also
- Clojure string functions: https://clojuredocs.org/clojure.string
- Introduction to Clojure: https://www.clojure.org/guides/learn/syntax
- Clojure cheatsheet: https://clojure.org/api/cheatsheet