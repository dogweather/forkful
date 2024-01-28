---
title:                "Refactoring"
date:                  2024-01-26T01:17:41.841861-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/refactoring.md"
---

{{< edit_this_page >}}

## Was & Warum?

Refactoring ist der Prozess der Umstrukturierung bestehenden Computer-Codes, ohne dessen externes Verhalten zu ändern, mit dem Ziel, nichtfunktionale Attribute zu verbessern. Programmierer nehmen ein Refactoring vor, um ihren Code sauberer, effizienter und leichter wartbar zu machen, wodurch die Lesbarkeit effektiv erhöht und die Komplexität ihrer Software reduziert wird.

## Wie:

Refactoring in Clojure—dank seiner klaren Syntax und des funktionalen Paradigmas—kann unglaublich unkompliziert sein. Betrachten wir ein häufiges Szenario: die Iteration über Kollektionen. Man könnte mit einer `for`-Schleife beginnen, so:

```clojure
(defn calculate-sum [numbers]
  (reduce + 0 numbers))

(defn old-way []
  (let [nums (range 1 11)]
    (calculate-sum nums)))
```

Das Aufrufen von `(old-way)` liefert uns 55, die Summe von 1 bis 10. Aber, hey, wir können dies zu einem mehr Clojure-esken Ansatz refaktorisieren:

```clojure
(defn new-way []
  (->> (range 1 11)
       (reduce +)))
```

Diese refaktorisierte Funktion `(new-way)` verwendet Threading-Makros, um den Bereich direkt in `reduce` zu leiten, den Ballast abwerfend.

## Vertiefung

Die Kunst des Refactorings hat ihre Wurzeln in den frühen Tagen der Softwareentwicklung, gewann aber wirklich an Zugkraft durch Martin Fowlers bahnbrechendes Buch "Refactoring: Improving the Design of Existing Code", veröffentlicht im Jahr 1999. In Clojure neigt das Refactoring oft dazu, sich auf die Prinzipien der funktionalen Programmierung zu stützen, wobei reine Funktionen und unveränderliche Datenstrukturen bevorzugt werden.

Alternativen zum manuellen Refactoring in Clojure könnten den Einsatz von Tools wie Cursive, ein beliebtes IntelliJ IDEA-Plugin, das automatisierte Refactorings speziell für Clojure bietet, umfassen. Es gibt auch clj-refactor, ein Emacs-Paket für Clojure, das eine Reihe von Refactoring-Funktionen bereitstellt.

Eine Herausforderung, die speziell beim Refactoring in Clojure auftritt, ist der Umgang mit Zustand und Nebenwirkungen in einem grundsätzlich unveränderlichen und nebenwirkungsfreien Paradigma. Die sorgfältige Verwendung von Atoms, Refs, Agents und Transients ist entscheidend, um sowohl die Leistung als auch die Korrektheit während des Refactorings zu erhalten.

## Siehe auch

- Martin Fowlers "Refactoring: Improving the Design of Existing Code" für die grundlegenden Konzepte.
- [Clojure Docs](https://clojuredocs.org/) für spezifische Beispiele von idiomatischem Clojure-Code.
- [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) für Automatisierung beim Refactoring in Emacs.
- [Cursive](https://cursive-ide.com/) für IntelliJ-Benutzer, die automatisierte Refactoring-Hilfe suchen.
- [Refactoring mit Rich Hickey](https://www.infoq.com/presentations/Simple-Made-Easy/) - Ein Vortrag des Schöpfers von Clojure, der, obwohl nicht speziell über Refactoring, Einblicke in die Clojure-Philosophie bietet, die effektive Refactoring-Entscheidungen leiten können.
