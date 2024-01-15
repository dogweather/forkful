---
title:                "Umwandlung eines Strings in Großbuchstaben"
html_title:           "Clojure: Umwandlung eines Strings in Großbuchstaben"
simple_title:         "Umwandlung eines Strings in Großbuchstaben"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Einige Programmieraufgaben erfordern die Großschreibung eines Strings. Das kann aus unterschiedlichen Gründen notwendig sein, beispielsweise für die korrekte Ausgabe in einem Nutzerinterface oder um bestimmte Formatierungsanforderungen zu erfüllen.

## Wie geht das?

Um einen String in Clojure zu kapitalisieren, gibt es eine einfache Funktion namens `capitalized`:

```Clojure
(capitalized "hallo") ; gibt "HALLO" aus
```

Die Funktion `capitalized` nimmt einen String als Argument und gibt den selben String in Großbuchstaben zurück.

Für fortgeschrittene Nutzer gibt es auch die Möglichkeit, die Funktion `caps-lock` zu nutzen, welche alle Buchstaben des Strings in Großbuchstaben umwandelt:

```Clojure
(caps-lock "hallo") ; gibt "HALLO" aus
```

## Tieferer Einblick

Die Funktion `capitalized` kann auch mehrere Strings akzeptieren und gibt diese dann alle in Großbuchstaben aus:

```Clojure
(capitalized "hello" "world") ; gibt "HELLO" "WORLD" aus
```

Zudem können auch andere Datentypen, wie zum Beispiel Vektoren, an `capitalized` übergeben werden und die Funktion gibt den Inhalt in Großbuchstaben aus:

```Clojure
(capitalized ["hello" "world"]) ; gibt ["HELLO" "WORLD"] aus
```

Es ist wichtig zu beachten, dass die Funktion `capitalized` nicht den ursprünglichen String verändert, sondern eine neu kapitalisierte Version zurückgibt.

## Siehe auch

- Clojure-Dokumentation für die Funktion `capitalized` (https://clojuredocs.org/clojure.core/capitalized)
- Eine Einführung in Clojure für Einsteiger (https://github.com/clojure/clojure/wiki/Getting-Started)

Vielen Dank fürs Lesen und viel Spaß beim Kapitalisieren von Strings in Clojure!