---
title:                "Concatenating strings"
aliases:
- /en/clojure/concatenating-strings/
date:                  2024-01-20T17:34:29.220521-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenating strings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings means sticking them end to end - "hello" + "world" becomes "helloworld". Programmers do this to build up text, like URLs, messages, or results based on user input or program data.

## How to:

Clojure makes string concatenation straightforward with the `str` function. Let's dive right in:

```clojure
;; Simple concatenation with the str function
(str "Hello, " "world!")
;; => "Hello, world!"

;; Concatenating multiple strings
(str "Clojure" " is" " awesome!")
;; => "Clojure is awesome!"

;; Combining strings and other values
(str "The answer is " 42)
;; => "The answer is 42"

;; Using apply to concatenate a sequence of strings
(apply str ["Join" " " "these" " " "strings!"])
;; => "Join these strings!"
```

Great, so you've seen it in action. Just remember `str` works with any value by calling `toString` on it. If it's nil, you get the string "nil".

## Deep Dive

Historically, string concatenation has been around since we needed to handle text programmatically, and each language offers its own methods. In Clojure, `str` is part of the core library, introduced for simplicity and uniformity.

Alternatives to `str`? Yes! `StringBuilder` can be more efficient for lots of concatenations, especially in loops. Clojure can call Java methods, so you can use `StringBuilder` as well:

```clojure
;; Using StringBuilder for efficiency
(let [builder (StringBuilder.)]
  (.append builder "This is")
  (.append builder " a more")
  (.append builder " efficient way!")
  (.toString builder))
;; => "This is a more efficient way!"
```

Why not always use `StringBuilder` then? For most everyday tasks, `str` is simpler and fast enough. `StringBuilder` shines in high-performance scenarios with many concatenations.

Implementation-wise, since Clojure is hosted on the JVM, it benefits from Java's string handling capabilities. However, like in Java `String`s, each `str` call creates a new `String`, which can be a memory consideration.

## See Also

- Clojure's `str` function documentation: [Clojure Strings](https://clojuredocs.org/clojure.core/str)
- Java's `StringBuilder`: [StringBuilder Docs](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/StringBuilder.html)
- Practical Clojure guide to `str` and more: [Clojure for the Brave and True](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
