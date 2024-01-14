---
title:    "Clojure: Substrings extrahieren"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstringen ist eine nützliche Funktion in der Programmierung. Es ermöglicht uns, bestimmte Teile von Strings zu isolieren und sie in verschiedenen Anwendungen zu verwenden.

## Wie man Teilstringe extrahiert

Um Teilstringe in Clojure zu extrahieren, können wir die Funktion `subs` verwenden. Sie akzeptiert zwei Argumente: den zu extrahierenden String und die Start- und Endpositionen des Teilstrings.

```Clojure
(def string "Hallo Welt!")
(subs string 0 5)
```

Dieser Code extrahiert den Teilstring "Hallo" aus dem Originalstring "Hallo Welt!".

Eine andere Möglichkeit Teilstringe zu extrahieren, ist die Verwendung von regulären Ausdrücken mit der Funktion `re-find`.

```Clojure
(def name "Max Mustermann")
(re-find #"Max" name)
```

Dieser Code extrahiert den Teilstring "Max" aus dem Namen "Max Mustermann".

## Tiefergehende Informationen

Um bestimmte Muster in einem String zu finden und zu extrahieren, können wir die Funktion `re-seq` verwenden. Diese Funktion akzeptiert einen regulären Ausdruck und einen String und gibt eine Sequenz aller Teilstrings zurück, die dem regulären Ausdruck entsprechen.

```Clojure
(def email "max.mustermann@example.com")
(re-seq #"@(.*)" email)
```

Dieser Code extrahiert die Domain aus der E-Mail-Adresse "max.mustermann@example.com" und gibt "example.com" zurück.

Es gibt auch Möglichkeiten, Teilstringe in mehreren Zeilen von Text zu extrahieren, indem `\n` im regulären Ausdruck verwendet wird, um einen Zeilenumbruch anzugeben.

## Siehe auch
- [Clojure Dokumentation für `subs`](https://clojuredocs.org/clojure.core/subs)
- [Clojure Dokumentation für `re-find`](https://clojuredocs.org/clojure.core/re-find)
- [Clojure Cheat Sheet für reguläre Ausdrücke](https://clojure.org/api/cheatsheet#fn-regex)