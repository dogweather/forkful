---
title:    "Clojure recipe: Capitalizing a string"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why Capitalize a String in Clojure?

Capitalizing a string, or converting the first letter of each word to uppercase, is a common task in many programming languages. In Clojure, this can be accomplished using built-in functions, making it a quick and easy process.

## How To Capitalize a String in Clojure

To capitalize a string in Clojure, you can use the `capitalize` function. This function takes in a string as input and returns a new string with the first letter of each word capitalized.

```
Clojure
(def str1 "hello world")
(capitalize str1)
```

The output of this code would be `"Hello World"`, as expected. It's important to note that the original string `str1` remains unchanged, as `capitalize` returns a new string rather than modifying the original.

If you want to capitalize only the first letter of the entire string, you can use the `capitalize-first` function. This function behaves similarly to `capitalize`, but only capitalizes the first letter of the first word.

To apply capitalization to multiple words, you can use the `clojure.string/capitalize-words` function. This function takes in a vector of strings as its argument and returns a new vector with the first letter of each string capitalized.

```
Clojure
(def str2 ["hello" "world"])
(clojure.string/capitalize-words str2)
```

The output of this code would be `["Hello" "World"]`.

## Deep Dive into Capitalizing a String in Clojure

Under the hood, the `capitalize` and `capitalize-first` functions use the `clojure.string/capitalize` function, which itself uses the `clojure.string/upper-case` function to convert the first letter to uppercase. This means that `capitalize` and `capitalize-first` are essentially shortcuts to save time and typing.

It's also worth noting that the `upper-case` function can be used on a single character, allowing you to capitalize the first letter of a string manually.

```
Clojure
(upper-case (first "hello"))
```

The output of this code would be `"H"`.

## See Also

- [Official Clojure Documentation on `capitalize`](https://clojure.github.io/clojure/branch-master/clojure.string-api.html#clojure.core/capitalize)
- [Clojure String Functions Cheat Sheet](https://gist.github.com/john2x/e1dca953548bfdfb9843)
- [Clojure for the Brave and True - Chapter 6: Strings](https://aphyr.com/posts/298-clojure-from-the-ground-up-6-strings)