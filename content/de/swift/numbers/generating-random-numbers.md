---
date: 2024-01-27 20:35:22.828334-07:00
description: "Die Erzeugung von Zufallszahlen in der Programmierung geht darum, nicht\
  \ deterministische oder unvorhersehbare numerische Werte zu kreieren. Programmierer\u2026"
lastmod: '2024-03-13T22:44:54.221424-06:00'
model: gpt-4-0125-preview
summary: "Die Erzeugung von Zufallszahlen in der Programmierung geht darum, nicht\
  \ deterministische oder unvorhersehbare numerische Werte zu kreieren. Programmierer\u2026"
title: Generierung von Zufallszahlen
weight: 12
---

## Was & Warum?

Die Erzeugung von Zufallszahlen in der Programmierung geht darum, nicht deterministische oder unvorhersehbare numerische Werte zu kreieren. Programmierer nutzen Zufallszahlen aus verschiedenen Gründen, beispielsweise um Unvorhersehbarkeit in Spielen zu simulieren, zufällige Stichproben aus Datensätzen auszuwählen oder für kryptografische Zwecke.

## Wie man es macht:

Swift bietet eine unkomplizierte Weise, Zufallszahlen durch seine Standardbibliothek zu generieren. Hier ist, wie man es für unterschiedliche numerische Typen macht:

```Swift
// Erzeugt eine zufällige Ganzzahl zwischen 0 und Int.max
let randomInt = Int.random(in: 0...Int.max)
print(randomInt)

// Erzeugt eine zufällige Fließkommazahl zwischen 0,0 und 1,0
let randomDouble = Double.random(in: 0.0...1.0)
print(randomDouble)

// Erzeugt einen zufälligen Bool-Wert
let randomBool = Bool.random()
print(randomBool)
```

Die Beispiel-Ausgabe könnte variieren, denn schließlich haben wir es mit Zufälligkeit zu tun. Das mehrmalige Ausführen des Codes wird unterschiedliche Zahlen und Boolesche Werte liefern.

## Vertiefung

Swifts Ansatz zur Zufallszahlengenerierung baut auf einem robusten und effizienten Pseudozufallszahlengenerator (PRNG) auf. Vor Swift 4.2 waren Entwickler auf externe Bibliotheken oder die Fähigkeiten der zugrundeliegenden Plattform angewiesen, was zu Inkonsistenzen über verschiedene Plattformen und Umgebungen führen konnte. Mit der Einführung von nativen APIs in Swift 4.2 wurde die Generierung von Zufallszahlen sowohl einfacher als auch konsistenter, unabhängig von der zugrundeliegenden Plattform.

Es ist jedoch kritisch zu verstehen, dass der standardmäßige Zufallszahlengenerator in Swift nicht für kryptografische Zwecke geeignet ist. Für die Kryptografie sollten Entwickler das `Security` Framework auf Apple-Plattformen verwenden, das Zugang zu kryptografisch sicheren Zufallsbytes bietet. Nach meinem letzten Stand umfasst Swift keinen plattformübergreifenden kryptografischen Zufallszahlengenerator in seiner Standardbibliothek, was Entwickler auf Nicht-Apple-Plattformen dazu veranlasst, nach Drittanbieterbibliotheken für solche Bedürfnisse zu suchen.

Im Bereich der wissenschaftlichen Berechnungen oder in Situationen, die eine deterministische Sequenz von pseudozufälligen Zahlen erfordern (wobei die Sequenz genau reproduziert werden kann), ist Swifts Zufallszahlenerzeugung möglicherweise nicht die beste Wahl ohne die Möglichkeit, den Generator zu initialisieren. In solchen Fällen werden oft spezialisierte Bibliotheken und Algorithmen eingesetzt, um diese präzisen Anforderungen zu erfüllen.
