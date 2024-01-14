---
title:                "Clojure: Suchen und Ersetzen von Text"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Suchen und Ersetzen von Text ist ein häufiges Problem, dem man beim Programmieren begegnet. In diesem Blogbeitrag werden wir uns ansehen, wie man dieses Problem in Clojure lösen kann.

## Anleitung

Um Text in Clojure zu suchen und zu ersetzen, können wir die Funktion `clojure.string/replace` verwenden. Hier ist ein Beispiel, wie man alle Vokale in einem String durch Sternchen ersetzen kann:

```Clojure
(def text "Hallo Welt")
(clojure.string/replace text #"[aeiou]" "*")
```
Dieser Code gibt `"H*ll* W*lt"` als Output zurück.

Um alle Vorkommen eines Wortes in einem String zu ersetzen, können wir folgenden Code verwenden:

```Clojure
(def text "Ich mag Äpfel, aber keine Bananen")
(clojure.string/replace text #"Äpfel" "Birnen")
```
Dies gibt `"Ich mag Birnen, aber keine Bananen"` zurück.

## Tiefentauchen

`clojure.string/replace` akzeptiert reguläre Ausdrücke als Suchmuster. Dadurch können wir komplexere Such- und Ersetzungsoperationen durchführen. Außerdem gibt es auch die Funktion `clojure.string/replace-first`, die nur das erste Vorkommen des Suchmusters ersetzt.

Eine andere nützliche Funktion ist `clojure.string/replace-first-regular`, die ähnlich wie `clojure.string/replace` funktioniert, aber reguläre Ausdrücke verwendet.

## Siehe auch

- [Dokumentation zu clojure.string/replace](https://clojuredocs.org/clojure.string/replace)
- [Dokumentation zu regulären Ausdrücken in Clojure](https://clojuredocs.org/clojure.core/re-seq)