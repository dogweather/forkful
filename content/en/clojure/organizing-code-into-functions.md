---
title:                "Organizing code into functions"
date:                  2024-01-25T02:59:30.238639-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizing code into functions"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## What & Why?

Chunking code into functions is about packaging up blocks of code that accomplish specific tasks. Doing this makes your code clean, easier to maintain, and a breeze for other devs to read.

## How to:

Clojure functions are defined with `defn`, followed by a name, parameters, and body. Here's a quick example.

```Clojure
(defn greet [name]
  (str "Hello, " name "!"))

(greet "Alex") ; => "Hello, Alex!"
```

Now let's say we want to calculate the area of a rectangle. Instead of bungling it all together, we separate it into two functions:

```Clojure
(defn area [length width]
  (* length width))

(defn print-area [length width]
  (println "The area is:" (area length width)))

(print-area 3 4) ; => The area is: 12
```

## Deep Dive

Way back, coders would just smash all their logic into a single block. It was ugly. Then structured programming came along, and functions turned into a thing. In Clojure, every function is first-class—you can sling them around like any other value.

Alternatives? Some folks might mess with multi-methods or higher-order functions, but those are just spices in the function stew.

All in a function's details: they're immutable in Clojure, making side-effect muddles less likely. They lean heavily on recursion instead of typical loops, which meshes well with the language's functional paradigms.

## See Also

- Clojure's own guide: https://clojure.org/guides/learn/functions
- Functional Programming Basics: https://www.braveclojure.com/core-functions-in-depth/
- Rich Hickey's Talks: https://changelog.com/posts/rich-hickeys-greatest-hits - for insight on Clojure's philosophy.