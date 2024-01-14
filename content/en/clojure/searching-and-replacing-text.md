---
title:                "Clojure recipe: Searching and replacing text"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Text manipulation is a common task when it comes to programming. Whether you're working on a web application or parsing data, the ability to search and replace text can be a huge time saver. In Clojure, this task can be easily accomplished with the help of built-in functions and libraries.

## How To
Searching and replacing text in Clojure can be done in a few simple steps. Let's take a look at some examples using the `clojure.string` library.

First, we need to require the library in our namespace using the `require` function:

```Clojure
(require '[clojure.string :as str])
```

Next, let's assume we have a string that we want to modify:

```Clojure
(def text "Hello, world!")
```

To replace a specific word or phrase in the string, we can use the `replace` function:

```Clojure
(str/replace text "world" "Clojure")  ;; output: Hello, Clojure!
```

We can also use regular expressions to find and replace text. For example, let's replace all vowels with the letter "x":

```Clojure
(str/replace text #"["aeiou]" "x")  ;; output: Hxllx, wxrld!
```

Additionally, we can use the `replace-first` function to only replace the first occurrence of a word or phrase:

```Clojure
(str/replace-first text "o" "a")  ;; output: Hella, world!
```

The `replace` and `replace-first` functions can also be used to replace text in a specific part of the string, by providing a starting and ending index. For example, let's replace the word "world" with "Clojure" starting at index 7:

```Clojure
(str/replace text #"world" "Clojure" 7)  ;; output: Hello, Clojure!
```

For more complex replacements, we can use the `replace-first` function with a function as its second argument. This function will receive the match as its parameter and can return the replacement value. Let's convert all letters in our string to uppercase:

```Clojure
(str/replace-first text #"[A-Za-z]" #(str/upper-case %))  ;; output: HELLO, WORLD!
```

## Deep Dive
Under the hood, the `replace` and `replace-first` functions use Java's `java.util.regex.Matcher` class for their regular expression matching. This allows for more powerful and customizable replacements. The `replace` function also has an optional `limit` parameter to specify the maximum number of replacements to be made.

Additionally, Clojure's `replace` function supports a third argument of type `java.util.function.Function` for more complex replacements, similar to using a function with `replace-first`.

There are also other libraries available, such as `clojure.string.replacements` and `clojure.string.replace`, for even more text manipulation functions and options.

## See Also
- [Clojure.string Documentation](https://clojure.github.io/clojure/clojure.string-api.html)
- [Java's java.util.regex.Matcher Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Matcher.html)
- [Clojure's replace and replace-first functions source code](https://github.com/clojure/clojure/blob/master/src/clj/clojure/string.clj#L236-L237)