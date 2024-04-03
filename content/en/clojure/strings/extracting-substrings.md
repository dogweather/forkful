---
date: 2024-01-20 17:45:33.645264-07:00
description: "Extracting substrings means snatching a specific part of a string\u2014\
  like getting the good bits out of a sandwich. Programmers do this to isolate data,\u2026"
lastmod: '2024-03-13T22:44:59.735546-06:00'
model: gpt-4-1106-preview
summary: "Extracting substrings means snatching a specific part of a string\u2014\
  like getting the good bits out of a sandwich."
title: Extracting substrings
weight: 6
---

## How to:
Clojure makes it easy to work with strings. For extracting substrings, `subs` is your go-to function:

```clojure
(let [text "ClojureRocks"]
  (subs text 7)) ; => "Rocks"

(let [text "ClojureRocks"]
  (subs text 0 7)) ; => "Clojure"
```

And that's it—give it a start index, and optionally an end index, and you'll chop the string just how you need it.

## Deep Dive
Extracting substrings isn't new—been around since the early days of programming. In Clojure, `subs` is a straightforward function. It's part of Clojure's Java interop capabilities, piggybacking on Java's `substring` method. Two key points: negative indices aren't allowed, and it's zero-based (starts counting at zero). So remember that or you'll be one off.

Alternatives? Sure. Regex with `re-find` and `re-matcher` for complex patterns, or `split` if you're dividing at a delimiter. Each tool has its place, but nothing beats `subs` for simplicity.

Implementation-wise, `subs` doesn't copy characters, it shares the original string's character array. Efficient, but if your original string is huge and all you need is a tiny bit, you might inadvertently keep the whole big string in memory.

## See Also:
- Official Clojure String API: [clojure.string](https://clojuredocs.org/clojure.string)
- Java `substring`: Because that's the powerhouse behind `subs`. [Java substring](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,%20int))
- Regular expressions in Clojure: [re-find](https://clojuredocs.org/clojure.core/re-find)
- Splitting strings in Clojure: [split](https://clojuredocs.org/clojure.string/split)
