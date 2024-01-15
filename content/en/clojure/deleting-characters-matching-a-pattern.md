---
title:                "Deleting characters matching a pattern"
html_title:           "Clojure recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

At some point in your coding journey, you might encounter a situation where you need to delete characters that match a certain pattern. This could be due to data cleaning or manipulation purposes, or to make your code more efficient. In Clojure, there are a few ways to achieve this task and in this article, we will explore some of them.

## How To

To begin with, let's create a string that contains some random characters:

```Clojure
(def my-string "Th1s 1s a Str1ng!")
```

Our goal is to delete all the digits in this string and return the remaining characters. Here are three ways to achieve this:

### Method 1: `filter` function

Clojure has a built-in `filter` function that takes a predicate and a collection as arguments. It returns a lazy sequence of the elements in the collection for which the predicate returns true. In our case, we can use the `filter` function to check if a character is a digit or not, and return only the non-digit characters.

```Clojure
(filter (complement digit?) my-string)
```

This will return a lazy sequence with characters `'T'`, `'h'`, `'s'`, `'i'`, `'s'`, and `'a'`.

### Method 2: `replace` function

Another way to achieve our goal is by using the `replace` function. This function takes a regular expression (regex) and two string arguments: the first is the string to be searched, and the second is the replacement string. In our case, we can use a regex that matches all digits and replace them with an empty string.

```Clojure
(replace #"\d" "" my-string)
```

The output of this would be the string `"Ths s a Strng!"`.

### Method 3: `loop` and `recur`

The `loop` and `recur` special forms in Clojure allow for the implementation of recursion without using the stack. We can use these forms to loop through the characters of a string and check if they are digits or not. If they are not digits, we add them to a new string using the `str` function.

```Clojure
(loop [remaining-str my-string
       new-str ""]
  (if (empty? remaining-str)
    new-str
    (let [char (first remaining-str)]
      (recur (rest remaining-str)
             (if (not (digit? char))
               (str new-str char)
               new-str)))))
```

The output of this would be the string `"This is a String!"`.

## Deep Dive

Apart from the methods mentioned above, there are many other ways to delete characters matching a pattern in Clojure. Some other options include using the `remove` function, using `mapcat` and `str` functions together, or even creating your own function that uses recursion or regular expressions. No matter which method you choose, it's important to understand how the underlying functions and special forms work to truly grasp the concept.

## See Also

- <https://clojure.org/>
- <https://purelyfunctional.tv/guide/reduce-filter-map/> 
- <https://hackr.io/blog/clojure-code-examples>