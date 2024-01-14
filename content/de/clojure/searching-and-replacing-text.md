---
title:    "Clojure: Suchen und Ersetzen von Text"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Warum

Das ersetzen von Text ist eine häufige Aufgabe beim Programmieren, insbesondere wenn es darum geht, Textdateien oder Strings zu manipulieren. In diesem Blog-Beitrag werden wir erkunden, wie man diese Aufgabe effizient in Clojure bewältigen kann.

## Wie geht das

Um Text in Clojure zu suchen und ersetzen, können wir die in der Standardbibliothek integrierten Funktionen verwenden. Zunächst müssen wir auch die `clojure.string` Bibliothek importieren, um auf nützliche Funktionen wie `replace` und `replace-first` zugreifen zu können.

```
(require '[clojure.string :as str]) ; Importieren der clojure.string Bibliothek

(str/replace "Hallo Welt" #"Welt" "Clojure") ; Suche und ersetze "Welt" mit "Clojure"
; => "Hallo Clojure"

(str/replace-first "Hello world" #"world" "Clojure") ; Suche und ersetze nur die erste Übereinstimmung
; => "Hello Clojure"
```

Wir können auch Platzhalter verwenden, um Teile des zu suchenden Textes dynamisch zu gestalten.

```
(def name "Alice")

(str/replace "Hallo ${name}" #"${name}" "Bob") ; Verwende den Platzhalter, um "Alice" durch "Bob" zu ersetzen
; => "Hallo Bob"
```

## Tiefer eintauchen

Die `replace` Funktion akzeptiert auch eine Funktion als dritten Parameter, die auf jede Übereinstimmung angewendet wird. Zum Beispiel können wir die Funktion `upcase` verwenden, um alle Übereinstimmungen in Großbuchstaben zu konvertieren.

```
(str/replace "hello world" #"hello" (fn [match] (str/upper-case match)))
; => "HELLO world"
```

Es gibt auch andere nützliche Funktionen in der `clojure.string` Bibliothek wie `replace-first` und `replace-all`, die es uns ermöglichen, bestimmte Teile des Textes zu suchen und ersetzen.

## Siehe auch

- Die offizielle Clojure-Dokumentation zu `clojure.string`: https://clojuredocs.org/clojure.string
- Ein Tutorial zum Suchen und Ersetzen mit regulären Ausdrücken in Clojure: https://www.braveclojure.com/string-escaping/