---
title:                "Finding the length of a string"
html_title:           "Clojure recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Finding the length of a string is a common task in many programming languages, including Clojure. It allows you to manipulate and work with strings in a more dynamic and efficient way. Whether you want to check the length of a user input or count the number of characters in a string, understanding how to find the length of a string in Clojure can be a useful skill to have.

## How To

To find the length of a string in Clojure, we can use the `count` function. This function takes in a sequence or collection and returns the number of elements in it. Since a string is a sequence of characters, we can easily find its length using this function. Let's see an example:

```
Clojure (count "Hello") ;; Output: 5
Clojure (count "I love Clojure") ;; Output: 14
```

As you can see, the `count` function returns the correct length of the string, including spaces and punctuation marks.

We can also use the `str` function to convert a string into a sequence and then use the `count` function on it. This can be useful in cases where we want to count only a specific set of characters in the string. For example:

```
Clojure (count (str "purple" "rain")) ;; Output: 11
;; Here, the length of the string "purple" is 6 and the length of "rain" is 4. By using the `str` function, we have concatenated the two strings and the result is 11.
```

## Deep Dive

Behind the scenes, the `count` function works by delegating to the `count` method on the passed sequence or collection. This means that we can use the `count` method directly on strings as well:

```
Clojure (.count "Clojure is awesome") ;; Output: 19
```

The `count` method is also used to find the length of other types of collections in Clojure, such as lists, vectors, and maps. However, it's important to note that the `count` method works in constant time for strings, but in linear time for other types of collections.

## See Also

- Official documentation for the `count` function in Clojure: https://clojuredocs.org/clojure.core/count
- A tutorial on working with strings in Clojure: https://www.baeldung.com/clojure-strings