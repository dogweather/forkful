---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Clojure: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum
Hast du jemals mit Texten gearbeitet und dich gefragt, wie du bestimmte Muster oder Wörter daraus extrahieren kannst? Mit regulären Ausdrücken (auch bekannt als Regex) kannst du genau das tun! Sie sind ein leistungsstarkes Werkzeug für die Mustererkennung in Texten und können in nahezu jeder Programmiersprache verwendet werden. Lass uns sehen, wie man sie in Clojure anwendet.

## Wie geht's
```Clojure
;; Erstelle ein reguläres Ausdrucksmuster, das alle Wörter mit 3 Buchstaben oder weniger in einem String findet.
(def pattern #"\b\w{1,3}\b")

;; Wende das Muster auf einen String an
(re-find pattern "Der Hund bellt.") 
;=> "Der"

;; Verwende das "find-all" Flag, um alle Übereinstimmungen in einem String zu finden
(re-seq pattern "Hund Katze Maus") 
;=> ("Hund" "Katze")

;; Verwende Capture Groups, um bestimmte Teile des Musters zu extrahieren
(def pattern #"(Hund|Katze|Pferd)")
(re-find pattern "Das Pferd galoppiert.") 
;=> "Pferd"

;; Verwende geänderte Symbole, um spezifische Zeichen zu suchen
(def pattern #"https://www\.beispiel\.de")
(re-find pattern "Die URL ist https://www.beispiel.de/index.html") 
;=> "https://www.beispiel.de"
```

## Tiefer Einblick
Reguläre Ausdrücke können auch verwendet werden, um Texte zu filtern, zu ersetzen oder zu validieren. Sie bieten auch die Möglichkeit, Variablen und Funktionen in Muster zu integrieren, was ihre Funktionalität noch weiter erhöht. Es gibt vielfältige Anwendungsmöglichkeiten für reguläre Ausdrücke und sie können in nahezu allen Bereichen der Softwareentwicklung nützlich sein.

## Siehe auch
- [Reguläre Ausdrücke in Clojure](https://clojure.org/guides/regular_expression)
- [Einführung in reguläre Ausdrücke in Clojure](https://www.braveclojure.com/regular-expressions/)
- [Reguläre Ausdrücke Tutorial in Clojure](http://tutorials.jenkov.com/clojure/regular-expressions.html)