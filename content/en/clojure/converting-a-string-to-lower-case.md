---
title:    "Clojure recipe: Converting a string to lower case"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why

Converting strings to lower case is a common operation in many programming languages. In Clojure, this task can be performed easily using built-in functions or by writing a custom function. But why would someone want to do this in the first place? 

In certain scenarios, it may be necessary to convert all characters in a string to lower case for consistency and easier comparison. For example, when working with user input, it is important to handle any casing variations to prevent errors or unexpected behavior.

## How To

To convert a string to lower case in Clojure, we can use the `lower-case` function from the `clojure.string` namespace.

```Clojure
(require '[clojure.string :as str])

(str/lower-case "HELLO WORLD") ; => "hello world"
```

We can also use the `clojure.string/lower-case` alias for shorter code:

```Clojure
(str/lower-case "GREETINGS") ; => "greetings"
```

If we need to convert multiple strings to lower case, we can use the `map` function to apply the `lower-case` function to each element in a sequence:

```Clojure
(map str/lower-case ["Clojure" "TUTORIAL"]) ; => ("clojure" "tutorial")
```

In addition to built-in functions, we can also write a custom function to convert strings to lower case. Here is an example using recursion:

```Clojure
(defn lower-case-rec [string]
  (if (empty? string)
    ""
    (str (lower-case (first string)) (lower-case-rec (rest string)))))

(lower-case-rec "CuRsIvE") ; => "cursive"
```

## Deep Dive

The `lower-case` function uses Unicode rules for case conversion, which means it is not limited to just ASCII characters. This can be useful when handling international or multilingual data.

We can also use the `lower-case` function with keywords, as they are also considered strings in Clojure. However, it is important to note that keywords will stay in their original form and only their string representation will be converted to lower case:

```Clojure
(str/lower-case :CLOJURE) ; => ":clojure"
```

If we need to handle strings with accents or special characters, we can use the `:locale` parameter to specify the language and locale or the `:strength` parameter to control the level of case sensitivity.

## See Also

- [Clojure String Functions](https://clojuredocs.org/clojure.string)
- [Unicode Standard for Case Mappings](https://www.unicode.org/unicode/reports/tr21/tr21-5.html)