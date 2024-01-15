---
title:                "Concatenating strings"
html_title:           "Clojure recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

If you're working with text-based data, chances are you'll need to combine different strings together at some point. Whether you're creating a sentence, a user input prompt, or building a URL for an API call, string concatenation is a useful and necessary skill to have when programming.

## How To

Concatenating strings in Clojure is a simple process. First, you'll need to define the strings you want to combine. Let's use the example of creating a sentence using user input. 

```Clojure
(def name "John")
(def age 25)
(def sentence (str "Hello, my name is " name " and I am " age " years old."))
```

The `def` function is used to define variables, in this case, `name` and `age` are strings while `sentence` is a combination of these strings using the `str` function. 

When we call `sentence`, the output will be "Hello, my name is John and I am 25 years old."

## Deep Dive

In Clojure, the `str` function is used to concatenate strings. It takes any number of arguments and returns a string that is the result of concatenating them. It can also take other datatypes, such as numbers or booleans, and convert them to strings automatically.

Clojure also has a `join` function that can be used to concatenate strings with a separator character. This function is useful when combining strings from a collection of elements.

```Clojure
(def fruits ["apple" "banana" "pear"])
(def fruit-list (str/join ", " "My favorite fruits are: " fruits))
```

In this example, the `fruit-list` variable will contain the value "My favorite fruits are: apple, banana, pear". The first argument in the `str/join` function is the separator character, while the second argument is the initial string that the collected strings will be added to.

## See Also

- [ClojureDocs - Str function](https://clojuredocs.org/clojure.core/str)
- [ClojureDocs - Join function](https://clojuredocs.org/clojure.string/join) 
- [Clojure for the Brave and True](https://www.braveclojure.com/)