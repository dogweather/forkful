---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:35.522172-07:00
description: 'Hoe: In Clojure, om een string naar kleine letters om te zetten, gebruik
  je de `clojure.string/lower-case` functie. Kijk hoe eenvoudig het is.'
lastmod: '2024-03-13T22:44:50.405472-06:00'
model: gpt-4-0125-preview
summary: In Clojure, om een string naar kleine letters om te zetten, gebruik je de
  `clojure.string/lower-case` functie.
title: Een string omzetten naar kleine letters
weight: 4
---

## Hoe:
In Clojure, om een string naar kleine letters om te zetten, gebruik je de `clojure.string/lower-case` functie. Kijk hoe eenvoudig het is:

```clojure
(require '[clojure.string :as str])

(str/lower-case "Hello, World!") ; => "hello, world!"
```

De output is duidelijk:

```clojure
"hello, world!"
```

## Diepgaand
Historisch gezien is het omzetten van hoofdletters sinds het begin van de computerwetenschappen in gebruik om de verwerking van tekstgegevens te harmoniseren. In Clojure maakt de `clojure.string/lower-case` functie deel uit van de `clojure.string` bibliotheek, een verzameling van hulpmiddelen voor stringmanipulatie opgenomen in de kern van de taal.

Alternatieven voor `clojure.string/lower-case` omvatten het zelf schrijven van een functie door middel van het mappen met `char` manipulatie, maar dit is het wiel opnieuw uitvinden wanneer je een ingebouwde functie hebt die geoptimaliseerd en goed getest is.

Intern geeft `clojure.string/lower-case` het zware werk door aan de eigen `toLowerCase` methode van Java, aangezien Clojure draait op de Java Virtual Machine (JVM). Dit zorgt voor hoge prestaties aangezien het voordeel haalt uit Java's volwassen bibliotheken.

## Zie Ook
- Clojure's `clojure.string` API: https://clojuredocs.org/clojure.string
- Java's `String.toLowerCase()` methode: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase()
