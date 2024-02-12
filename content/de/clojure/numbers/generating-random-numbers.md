---
title:                "Generierung von Zufallszahlen"
aliases:
- /de/clojure/generating-random-numbers/
date:                  2024-01-27T20:33:10.348859-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generierung von Zufallszahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Generierung von Zufallszahlen in der Programmierung dreht sich darum, Werte zu erstellen, die im Voraus logisch nicht vorhergesagt werden können. Programmierer tun dies aus verschiedenen Gründen, einschließlich der Generierung einzigartiger Identifikatoren, der Simulation von Szenarien in der Spielentwicklung oder der Auswahl zufälliger Stichproben aus Daten zur Analyse.

## Wie:

In Clojure ist die Generierung von Zufallszahlen unkompliziert, und es gibt ein paar integrierte Funktionen, die sofort verwendet werden können.

Um eine zufällige Gleitkommazahl zwischen 0 (einschließlich) und 1 (ausschließlich) zu generieren, kann die Funktion `rand` verwendet werden:

```Clojure
(rand)
;; Beispieloutput: 0.7094245047062917
```

Wenn Sie eine Ganzzahl in einem spezifischen Bereich benötigen, verwenden Sie `rand-int`:

```Clojure
(rand-int 10)
;; Beispieloutput: 7
```

Dies liefert eine zufällige Ganzzahl zwischen 0 (einschließlich) und der Zahl, die Sie als Argument übergeben (ausschließlich).

Um eine zufällige Zahl innerhalb eines spezifischen Bereichs zu generieren (nicht auf Ganzzahlen beschränkt), können Sie `rand` mit Arithmetik kombinieren:

```Clojure
(defn rand-range [min max]
  (+ min (* (rand) (- max min))))
;; Nutzung
(rand-range 10 20)
;; Beispieloutput: 14.857457734992847
```

Diese Funktion `rand-range` gibt eine zufällige Gleitkommazahl zwischen den von Ihnen angegebenen `min`- und `max`-Werten zurück.

Für Szenarien, die komplexere Verteilungen oder Folgen von Zufallszahlen benötigen, bei denen Wiederholbarkeit notwendig ist (unter Verwendung von Seeds), könnte es sein, dass Sie sich zusätzliche Bibliotheken ansehen müssen, die über das Integrierte hinausgehen.

## Tiefergehend

Der zugrunde liegende Mechanismus zur Generierung von Zufallszahlen in den meisten Programmiersprachen, einschließlich Clojure, stützt sich typischerweise auf einen Pseudo-Zufallszahlengenerator (PRNG). Ein PRNG verwendet einen Algorithmus, um eine Folge von Zahlen zu produzieren, die die Eigenschaften von Zufallszahlen annähernd darstellt. Es ist erwähnenswert, dass diese, da sie algorithmisch erzeugt werden, nicht wirklich zufällig sind, aber für die meisten praktischen Zwecke ausreichend sein können.

In den frühen Tagen der Informatik war die Erzeugung hochwertiger Zufallszahlen eine bedeutende Herausforderung, was zur Entwicklung verschiedener Algorithmen führte, um Zufälligkeit und Verteilung zu verbessern. Für Clojure sind die integrierten Funktionen wie `rand` und `rand-int` praktisch für den alltäglichen Gebrauch und decken ein breites Spektrum an häufigen Anwendungsfällen ab.

Jedoch wenden sich Entwickler für Anwendungen, die kryptographische Sicherheit oder komplexere statistische Stichprobenmethoden erfordern, oft an externe Bibliotheken, die robustere und spezialisierte PRNGs anbieten. Bibliotheken wie `clj-random` bieten Zugang zu einer größeren Vielfalt von Algorithmen und größerer Kontrolle über die Saatgutzuteilung, was für Simulationen, kryptographische Anwendungen oder jeden Bereich, wo die Qualität und Vorhersagbarkeit der Zufallszahlensequenz bedeutende Auswirkungen haben könnte, entscheidend sein kann.

Obwohl Clojures integrierte Fähigkeiten zur Generierung von Zufallszahlen für viele Aufgaben ausreichend sind, kann die Erkundung externer Bibliotheken tiefere Einblicke und Optionen für maßgeschneiderte oder kritischere Anwendungen bieten.
