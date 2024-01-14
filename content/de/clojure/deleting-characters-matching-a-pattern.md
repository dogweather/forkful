---
title:                "Clojure: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum
In diesem Blogbeitrag werden wir uns damit beschäftigen, wie man in Clojure Zeichen löscht, die einem bestimmten Muster entsprechen. Das Löschen von Zeichen in einer Programmiersprache kann auf den ersten Blick vielleicht unsinnig erscheinen, aber es kann in bestimmten Situationen sehr nützlich sein.

## Wie geht das?
Um Zeichen zu löschen, die einem bestimmten Muster entsprechen, können wir die Clojure-Funktion `remove` verwenden. Diese Funktion nimmt eine Funktion als Parameter, die definiert, welches Zeichen gelöscht werden soll. Im Beispiel verwenden wir die `str/map`-Funktion, um ein Buchstaben-A-B-C zu erstellen. Danach geben wir das Ergebnis mit der `remove`-Funktion aus.

```Clojure
(str/map char \A \B \C)
;; gibt "ABC" aus
(remove #{\A\B} (str/map char \A \B \C))
;; gibt "C" aus
```
In diesem Beispiel geben wir eine Menge mit den Zeichen `\A` und `\B` an die `remove`-Funktion, die dann die entsprechenden Zeichen aus der Ausgabe löscht. Gewöhne dich an das Experimentieren mit verschiedenen Funktionen und Parametern, um ein besseres Verständnis für das Löschen von Zeichen in Clojure zu erhalten.

## Tiefere Einblicke
Das Löschen von Zeichen in Clojure mag auf den ersten Blick einfach erscheinen, aber es gibt verschiedene Methoden, um bestimmte Zeichen oder Muster zu löschen. Eine weitere Möglichkeit ist die Verwendung der `string-replace`-Funktion. Diese Funktion nimmt als Parameter ein reguläres Ausdrucksmuster und eine Ersatzzeichenfolge an, um bestimmte Zeichen zu ersetzen oder zu löschen. Hier ist ein Beispiel, um die Vokale aus einem String zu löschen:

```Clojure
(string-replace #"a|e|i|o|u" "Hello World!")
;; gibt "Hll Wrld!" aus
```

Es gibt auch die Möglichkeit, Zeichen aus einer Liste zu löschen, indem man die `filter`-Funktion verwendet. Diese Funktion nimmt ebenfalls eine Funktion als Parameter und gibt alle Elemente der Liste zurück, die diese Funktion erfüllen. Hier ist ein Beispiel, um alle Zahlen aus einer Liste von Zeichen zu löschen:

```Clojure
(filter #(not (number? %)) [\h \e \l \l \o \space 1 2 3])
;; gibt [\h \e \l \l \o \space] aus
```

Es gibt noch viele weitere Möglichkeiten, Zeichen in Clojure zu löschen, wie zum Beispiel die Verwendung der `clojure.string`-Bibliothek oder das Anwenden von `apply` auf eine Funktion, die Zeichen löscht. Experimentiere und finde heraus, welche Methode am besten zu deinem spezifischen Problem passt.

## Siehe auch
- [Clojure-Dokumentation für die `remove`-Funktion](https://clojuredocs.org/clojure.core/remove)
- [Manipulating Strings in Clojure](https://luminusweb.com/docs/clojure-manipulating-strings.html)
- [Clojure Cheat Sheet](https://clojure.org/api/cheatsheet)