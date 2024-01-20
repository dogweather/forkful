---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Search and Replace Text in Clojure

## What & Why?

Searching and replacing text is a task programmers often need; it involves scanning and substitifying portions of strings. Very convenient for data clean-up, pattern recognition and much more.

## How to:

In Clojure, use `clojure.string/replace` function for search and replace. Keyword arguments are: `:pattern` (what to search) and `:replacement`(what to replace it with).

```Clojure
(require '[clojure.string :as str])

(defn replace-text
  [text pattern replacement]
  (str/replace text pattern replacement))
```

Example input: 

```Clojure
(replace-text "Hello, Programmers" "Programmers" "World")
```

Example output:

```Clojure
"Hello, World"
```

## Deep Dive:

This concept's been around since early programming days. Stems from manual typesetting, where physical blocks of type were replaced.

Alternatives to `str/replace` in Clojure could be `re-seq` (returns a sequence of all matches) or using regular expressions (regex). But regex might be overkill for simple search-replace.

On implementation, `str/replace` functions will convert the pattern into a regex if not already, so there is a slight overhead to consider.

## See Also:

1. [Clojure Docs - clojure.string](https://clojuredocs.org/clojure.string)