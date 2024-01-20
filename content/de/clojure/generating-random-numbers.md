---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Generieren von Zufallszahlen ist ein Prozess, bei dem eine Reihe von Zahlen auf eine Weise erstellt wird, die nicht vorhersehbar ist. Programmierer machen das zur Simulation unvorhersehbarer Ereignisse oder zum Testen von Programmen durch Erzeugung zufälliger Eingaben.

## So geht's:

Mit Clojure können Sie auf einfache Weise Zufallszahlen generieren. Hier ist ein einfacher Code, um eine Zufallszahl zwischen 0 und 1 zu erzeugen:

```Clojure
(random)
```

Die Ausgabe ist eine Fließkommazahl zwischen 0 (einschließlich) und 1 (ausschließlich).

Um eine Zufallszahl innerhalb eines bestimmten Bereichs zu erzeugen, verwenden Sie `(+ (random) (* range x))`, wobei x die untere Grenze und range die Spanne der gewünschten Zahlen ist.

```Clojure
(defn random-range [x range]
 (+ x (* range (random))))

(random-range 10 20)
```

Die Ausgabe ist eine Fließkommazahl zwischen 10 und 30.

## Tiefere Einblicke:

Zufallszahlen haben eine lange Geschichte in der Informatik. Sie sind eine Schlüsselkomponente in vielen Bereichen, von Computerspielen bis hin zur Sicherheit. In Clojure werden Zufallszahlen durch eine deterministische Methode generiert, was bedeutet, dass sie reproduzierbar sind, wenn der Seed-Wert gleich bleibt. Alternativen zur eingebauten `random` Funktion könnten Bibliotheken wie `java.util.Random` oder `java.security.SecureRandom` sein, die weitere Konfigurationsmöglichkeiten bieten.

## Weiterführende Quellen:

Clojure hat immer noch viele Geheimnisse zu entdecken. Hier sind einige Ressourcen, um Ihnen auf Ihrer Reise zu helfen:

- [Clojure API-Dokumentation](https://clojure.github.io/clojure/)
- [Clojure - Getting started](https://clojure.org/guides/getting_started)
- [Clojure - About random](https://clojure.org/about/functional_programming)

Erkunden Sie und entwickeln Sie Ihr Verständnis für diese mächtige Programmiersprache.