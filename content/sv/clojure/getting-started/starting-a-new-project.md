---
date: 2024-01-20 18:03:12.540343-07:00
description: "Starta ett nytt projekt inneb\xE4r att skapa en ny bas d\xE4r din kod\
  \ kan v\xE4xa och utvecklas. Programmerare g\xF6r det f\xF6r att omvandla id\xE9\
  er till k\xF6rbar kod som\u2026"
lastmod: '2024-03-13T22:44:37.526097-06:00'
model: gpt-4-1106-preview
summary: "Starta ett nytt projekt inneb\xE4r att skapa en ny bas d\xE4r din kod kan\
  \ v\xE4xa och utvecklas. Programmerare g\xF6r det f\xF6r att omvandla id\xE9er till\
  \ k\xF6rbar kod som\u2026"
title: "Att p\xE5b\xF6rja ett nytt projekt"
---

{{< edit_this_page >}}

## Vad & Varför?
Starta ett nytt projekt innebär att skapa en ny bas där din kod kan växa och utvecklas. Programmerare gör det för att omvandla idéer till körbar kod som löser problem eller skapar värde.

## Hur man gör:
För att kickstarta ett Clojure-projekt använder vi Leiningen, en populär byggautomatisering och projektledningsverktyg för Clojure.

1. Installera Leiningen:
Besök [Leiningen's hemsida](https://leiningen.org/) och följ installationsinstruktionerna.

2. Skapa ett nytt projekt:
Öppna en terminal och kör:
```Clojure
lein new app mitt-kloj-projekt
```
Ersätt `mitt-kloj-projekt` med ditt projektnamn.

3. Projektets struktur:
Kontrollera den nya projektstrukturen:
```Clojure
tree mitt-kloj-projekt
```
Output ser ut ungefär så här:
```Clojure
mitt-kloj-projekt
├── project.clj
├── README.md
├── resources
├── src
│   └── mitt_kloj_projekt
│       └── core.clj
└── test
    └── mitt_kloj_projekt
        └── core_test.clj
```

4. Kör projektet:
Gå till projektets katalog och starta REPL:
```Clojure
cd mitt-kloj-projekt
lein repl
```
I REPL, kör:
```Clojure
(-main)
```
Du ser ett hälsningsmeddelande eller liknande från din `core.clj` fil.

## Fördjupning
Leiningen lanserades 2010 och är baserat på en tidigare verktyg som heter Cake. Det löser samma problemställningar som Maven och Gradle gör i Java-ekosystemet. Alternativ till Leiningen inkluderar Boot och den nyare verktyget tools.deps som är en del av Clojure CLI-verktyg. Leiningen hanterar beroenden, körnings- och testcykler samt paketering och distribution av applikationen. Den `project.clj` fil är hjärtat i ett Leiningen-projekt, som behåller alla konfigurationer.

## Se även
- Clojure's officiella hemsida: [https://clojure.org/](https://clojure.org/)
- Leiningen's användarmanual: [https://leiningen.org/](https://leiningen.org/)
- ClojureDocs, en gemenskapsdriven klargörande: [https://clojuredocs.org/](https://clojuredocs.org/)
- En guide till Boot: [http://boot-clj.com/](http://boot-clj.com/)
- Getting started with Clojure CLI tools: [https://clojure.org/guides/getting_started](https://clojure.org/guides/getting_started)
