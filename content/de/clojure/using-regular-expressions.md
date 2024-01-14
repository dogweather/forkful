---
title:    "Clojure: Die Verwendung von regulären Ausdrücken"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schon eine Weile mit Clojure programmierst, hast du vielleicht schon von regulären Ausdrücken gehört. Aber warum sollte jemand sie verwenden? Reguläre Ausdrücke sind eine nützliche Möglichkeit, Textmuster zu analysieren und zu manipulieren, was sie zu einem wertvollen Werkzeug in der Programmierung macht.

## Wie man sie verwendet

Um reguläre Ausdrücke in Clojure zu verwenden, müssen wir zuerst das `re-seq`-Modul importieren.

```Clojure
(ns meinprojekt.core
  (:require [clojure.string :as str]
            [clojure.re-seq :as re]))
```

Als nächstes können wir `re-seq` in Aktion sehen, indem wir eine Liste von Zeichenfolgen erstellen und ein reguläres Ausdrucksmuster angeben, nach dem wir suchen möchten.

```Clojure
(def emails ["john@example.com", "jane@domain.com", "foobar@server.net"])

; Wir suchen nach E-Mail-Adressen, die auf ".com" enden
(def pattern #"(.com)$")

; Wir können jetzt das re-seq-Modul verwenden, um die passenden E-Mail-Adressen zu extrahieren
(def matches (re-seq pattern emails))

; Die Ausgabe wird eine Liste mit den passenden E-Mail-Adressen sein
["john@example.com", "jane@domain.com"]
```

Wir können auch reguläre Ausdrücke verwenden, um Textmuster zu ersetzen oder zu ändern. Zum Beispiel können wir alle Vokale in einem String durch Sternchen ersetzen.

```Clojure
(def text "Reguläre Ausdrücke sind großartig!")

(def pattern #"[aeiou]")

; Wir verwenden `str/replace` mit unserem regulären Ausdrucksmuster und dem Ersatztext " * "
(str/replace text pattern " * ")

; Die Ausgabe wird "R*g*l*r* *sdr*ck* *nd *gr*ß*rt*g!" sein
```

## Tiefer Einblick

Reguläre Ausdrücke können natürlich viel komplexer sein und verschiedenste Anwendungen haben. Mit ihnen kannst du zum Beispiel E-Mail-Adressen, URLs, Telefonnummern oder sogar ganze Codeblöcke aus Texten extrahieren. Sie können auch verwendet werden, um Text zu validieren oder zu filtern, je nachdem was du brauchst.

Falls du tiefer in die Welt der regulären Ausdrücke eintauchen möchtest, gibt es viele Ressourcen im Internet, die dir dabei helfen können. Hier sind einige nützliche Links:

- [Clojure Dokumentation zu regulären Ausdrücken](https://clojure.org/reference/regular_syntax)
- [Regexr - Ein online Tool zum Erstellen und Testen von regulären Ausdrücken](https://regexr.com/)
- [Regular Expressions Cookbook für Clojure](https://github.com/maria-c/clojure-regular-expressions-cookbook)

## Siehe auch

- [Wie man mit Zeichenfolgen in Clojure arbeitet](https://www.maria-example.com/strings-in-clojure)
- [Einführung in das Parsing von Daten mit Instaparse in Clojure](https://www.maria-example.com/parsing-data-with-instaparse)