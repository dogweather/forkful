---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

String interpolation lets you inject data directly into a string. Want quick, clean variable substitutions in your strings? This is how you do it.

## How to:

Do string interpolation in Clojure like the example below. We're using the `str` function here to plug some variables (`name` and `age`) into our string.

```Clojure
(def name "Alice")
(def age 32)

; Print a statement
(println (str "Hi, I'm " name ", age " age "."))
```
Running this, you'll see:

```
Hi, I'm Alice, age 32.
```

Pretty straightforward, right? One function, and voilà, you have a neatly interpolated string.

## Deep Dive

Clojure, drawing from its Lisp roots, uses string concatenation instead of native string interpolation found in other languages like Python or Ruby.

**Historical context**: Back in Lisp's days, computation was more precious, so it was often more efficient to concatenate strings, opposed to interpolation.

**Alternatives**: Apart from `str`, you can do similar jobs using `format`. Just plug `%s` wherever you want a string and `%d` for integers. Then list your variables after the string.

```Clojure
(def name "Alice")
(def age 32)

(println (format "Hi, I'm %s, age %d." name age))
```
Gives you the same output, just via a slicker route.

**Implementation details**: String concatenation with `str` works across different data types without conversion. `format`, while more typist-friendly for long strings, needs explicit type handling – hence the `%s` and `%d`.

## See Also

For deeper understanding, swing by the official Clojure docs ([str](https://clojuredocs.org/clojure.core/str) and [format](https://clojuredocs.org/clojure.core/format)). And to appreciate Clojure's Lisp lineage, check out Paul Graham's essay, ["Beating the Averages"](http://www.paulgraham.com/avg.html). Happy coding!