---
title:                "Umwandeln eines Strings in Kleinbuchstaben"
html_title:           "Clojure: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man eine Zeichenkette in Kleinbuchstaben umwandeln? Nun, es gibt verschiedene Anwendungen, wie z.B. beim Vergleich von Benutzereingaben oder bei der Formatierung von Text in bestimmten Situationen.

## Wie geht das?
```Clojure
(def text "HALLO WELT!")
(println (clojure.string/lower-case text))
```
Output: "hallo welt!"

Um eine Zeichenkette in Kleinbuchstaben umzuwandeln, können wir die Funktion `lower-case` aus der `clojure.string` Bibliothek verwenden. Zuerst definieren wir eine Variable `text` mit dem Wert "HALLO WELT!" und dann geben wir den umgewandelten Text mit `println` aus. Bitte beachte, dass wir `clojure.string` vor dem Aufruf von `lower-case` mit einem "/" angeben und die Funktion als Argument die Variable `text` erhält.

## Tieferer Einblick
Um zu verstehen, wie die `lower-case` Funktion arbeitet, können wir einen genaueren Blick darauf werfen. Sie nutzt in Wahrheit die `clojure.core` Funktion `str/lower-case`, welche eine Zeichenkette auf die gleiche Weise in Kleinbuchstaben umwandelt. Die Funktion wandelt nicht nur einzelne Buchstaben, sondern auch alle Akzentzeichen und Sonderzeichen um, wie z.B. das "ß" in "ss" oder das "ä" in "ae".

## Siehe auch
- https://clojuredocs.org/clojure.string/lower-case
- https://clojuredocs.org/clojure.core/str/lower-case
- https://clojuredocs.org/clojure.core/println