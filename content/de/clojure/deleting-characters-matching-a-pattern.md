---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "Clojure: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Was & Warum?

Wenn wir von "Löschen von Zeichen, die einem Muster entsprechen" sprechen, meinen wir das Entfernen von bestimmten Zeichen aus einem String, die einem bestimmten Muster entsprechen. Programmierer tun dies, um unerwünschte Zeichen aus einer Zeichenkette zu entfernen oder um ein bestimmtes Format für die Zeichenkette zu erzwingen.

Wie geht's?

Um Zeichen, die einem Muster entsprechen, in Clojure zu löschen, können wir die Funktion "replace" mit dem Muster und einer leeren Zeichenkette als Argumente verwenden. Sehen wir uns ein Beispiel an:

```Clojure
(def string "Hallo! Wie geht es dir?")
(replace #"!" "" string)
```
Output: "Hallo Wie geht es dir?"

Hier haben wir das Ausrufezeichen mit Hilfe des regulären Ausdrucks #"!" gelöscht und eine leere Zeichenkette als Ersatz verwendet. Dies kann auch verwendet werden, um andere unerwünschte Zeichen zu entfernen, indem das entsprechende Muster verwendet wird.

Tiefeinsicht

Das Löschen von Zeichen, die einem Muster entsprechen, ist ein häufig verwendetes Verfahren in der Textverarbeitung und beim Parsing von Daten. Es kann hilfreich sein, um unerwünschte Zeichen aus Nutzereingaben zu entfernen oder um die Formatierung von Daten zu vereinheitlichen.

Eine alternative Methode zum Löschen von Zeichen ist die Verwendung der Funktion "trim" in Kombination mit der Funktion "replace". Diese Methode entfernt alle vorgegebenen Zeichen am Anfang und Ende des Strings und kann auch ein Muster als Argument verwenden.

Bei der Implementierung des regulären Ausdrucks #"!" werden die Zeichen in einer Zeichenkette Schritt für Schritt überprüft und jedes Zeichen, das dem Muster entspricht, wird durch die angegebene Ersatzzeichenkette ersetzt.

Siehe auch

- Clojure-Dokumentation für "replace": https://clojuredocs.org/clojure.core/replace
- Vergleich von regulären Ausdrücken in Clojure: https://www.oreilly.com/library/view/introducing-clojure/9781449383067/ch04.html#sec_regular
- Beispiele für die Verwendung von "trim" und "replace" in Clojure: https://www.javatips.net/api/prism-clojure-master/src/main/java/com/cheney/prism/gleaners/clojure/clj/graphics/mc/gleaners/filter/original/CustomChapter2.java