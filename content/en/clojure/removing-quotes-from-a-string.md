---
title:                "Removing quotes from a string"
date:                  2024-01-25T20:50:40.718672-07:00
model:                 gpt-4-1106-preview
simple_title:         "Removing quotes from a string"

category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Removing quotes from a string means getting rid of those pesky double or single quote characters that wrap your text. Programmers do this to cleanse data, ensure uniformity, or prepare strings for processing where quotes are undesired or may cause errors.

## How to:
In Clojure, strings are immutable, so when we talk about "removing quotes," we're really talking about creating a new string sans quotes. Here's the skinny using `clojure.string/replace`:

```clojure
(require '[clojure.string :as str])

; Let's ditch those double quotes
(defn remove-double-quotes [s]
  (str/replace s #"\"" ""))

; And kick out the single quotes
(defn remove-single-quotes [s]
  (str/replace s #"\'" ""))

; Sample usage:
(remove-double-quotes "\"Hello, World!\"") ; => "Hello, World!"
(remove-single-quotes "'Hello, World!'")   ; => "Hello, World!"
```
Wanna handle both single and double quotes in one fell swoop? Peep this:

```clojure
(defn remove-quotes [s]
  (str/replace s #"[\"\']" ""))

; Sample usage:
(remove-quotes "\"Hello, 'Clojure' World!\"") ; => "Hello, Clojure World!"
```

## Deep Dive
Back in the day when data was messier than a kid's bedroom, quotes in strings were the norm for denoting text. But as computer science evolved, quotes became more than just text delimiters—they took on syntactical roles in programming languages.

Clojure, with its Lisp heritage, doesn't use quotes the same way as some other languages might. They’re used for denoting strings for sure, but they also have a special role in creating literals. Regardless, removing quotes from strings remains a timeless task.

Why not just slice off the ends of a string? Well, that's assuming your quotes are always hugging the start and end of your string like a pair of overly affectionate grandparents. Real world data is messier. Enter regex (regular expressions), which lets you target those quotes no matter where they're hiding.

Alternatives? Sure, you can get fancy with `subs`, `trim`, `triml`, `trimr`, or even transducers if you want to show off. But `replace` with regex is like bringing a lightsaber to a knife fight—it cuts right to the chase.

## See Also
If your brain's itching for more Clojure string manipulation goodness, these breadcrumbs might help:

- ClojureDocs on `clojure.string/replace`: https://clojuredocs.org/clojure.string/replace
- Regular expressions in Clojure: https://clojure.org/guides/learn/syntax#_regex
- Java interop for string handling (Clojure runs on the JVM after all): https://clojure.org/reference/java_interop#_working_with_strings

Don’t just stop at removing quotes. There’s a whole world of string wizardry out there in Clojure-land waiting to be discovered.
