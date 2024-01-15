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

## Warum

Manchmal müssen wir in unseren Programmen bestimmte Zeichen löschen, zum Beispiel Leerzeichen oder Sonderzeichen. Das kann aus verschiedenen Gründen notwendig sein, zum Beispiel um Daten zu bereinigen oder um die Eingabe des Benutzers zu überprüfen.

## Wie man es macht

In Clojure gibt es verschiedene Möglichkeiten, um Zeichen basierend auf einem Muster zu löschen. Eine einfache und effektive Möglichkeit ist die Verwendung von regulären Ausdrücken.

```
;; Beispielcode zum Löschen von Leerzeichen aus einer Zeichenkette
(def text "Dies ist ein Beispieltext.")
(def neuer-text (clojure.string/replace text #" " ""))
;; Ausgabe: "DiesisteinBeispieltext."
```

In diesem Beispiel verwenden wir die Funktion `clojure.string/replace`, um alle Leerzeichen in der Zeichenkette `text` mit dem leeren String `""` zu ersetzen. Das `#` vor dem Muster `" "` zeigt an, dass es sich um einen regulären Ausdruck handelt, der alle Leerzeichen in der Zeichenkette matcht.

Eine weitere mögliche Lösung ist die Verwendung der Funktion `clojure.string/trim`. Diese Funktion entfernt alle Leerzeichen am Anfang und Ende einer Zeichenkette.

```
(def text "   Ein Beispieltext   ")
(def neuer-text (clojure.string/trim text))
;; Ausgabe: "Ein Beispieltext"
```

Diese Methode ist nützlich, wenn wir nur Leerzeichen am Anfang oder Ende der Zeichenkette entfernen möchten. Wenn wir jedoch spezifische Zeichen oder Muster entfernen möchten, ist die Verwendung von regulären Ausdrücken die bessere Wahl.

## Tiefere Einblicke

Clojure bietet noch viele weitere Funktionen und Möglichkeiten, um Zeichen basierend auf einem Muster zu löschen. Hier sind einige weitere nützliche Funktionen:

- `clojure.string/replace-first`: Entfernt nur das erste Auftreten des Musters und lässt alle anderen unberührt.
- `clojure.string/replace-nth`: Entfernt das Muster nur an der angegebenen Stelle in der Zeichenkette.
- `clojure.string/replace-last`: Entfernt nur das letzte Auftreten des Musters.
- `clojure.string/replace-regex`: Führt die Ausführung eines regulären Ausdrucks auf der Zeichenkette aus.

Es gibt auch die Möglichkeit, benutzerdefinierte Funktionen zu erstellen, um spezifische Zeichen oder Muster zu entfernen. Das Erlernen von regulären Ausdrücken ist in diesem Zusammenhang sehr hilfreich, da wir dadurch mehr Kontrolle über den Löschvorgang haben.

## Siehe auch

- [Clojure Dokumentation zu regulären Ausdrücken](https://clojuredocs.org/clojure.string/replace-regex)
- [Tutorial zu regulären Ausdrücken in Clojure](https://www.braveclojure.com/regular-expressions/)