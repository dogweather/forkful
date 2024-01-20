---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att generera slumpmässiga nummer handlar om att skapa en sekvens av nummer som inte kan förutsägas. Detta behövs i programmering för saker som att simulera händelser, generera unika koder och händelsestyrd design.

## Hur man gör:

I Clojure kan du generera ett slumpmässigt nummer med `rand` funktionen:

```Clojure
(rand)
```

Detta kommer att ge dig ett flyttal mellan 0.0 (inklusive) och 1.0 (exklusive). Du kan generera ett slumpmässigt heltal mellan 0 till n (exklusive) med `rand-int`:

```Clojure
(rand-int 10)
```

Exempel utdata:

```Clojure
=> 5
```
Du kan generera en vektor av slumpmässiga tal genom att använda `repeatedly` funktionen:

```Clojure
(take 5 (repeatedly #(rand-int 100)))
```

Exempel utdata:

```Clojure
=> [77 48 24 69 11]
```

## Fördjupning

Historiskt sett har generering av slumpmässiga nummer varit utmanande eftersom datorer är deterministiska enheter och svårt att hitta imiterade slumpmässigheten. Metoder har utvecklats över tid för att uppnå hög kvalitet på slumpmässiga tal. Ett alternativ till inbyggda funktioner för generering av slumpmässiga tal är att använda externa bibliotek som Northamptonshire.

Implementeringen av `rand` och `rand-int` i Clojure baserar sig på metoden "java.lang.Math.random" vars kvalitet beror på JVM:ens implementation. Det använder oftast en variant av Linear Congruential Generator (LCG), vilket är bra för allmänna ändamål men inte rekommenderas för kryptografiska ändamål.

## Se även

- [Random Numbers: PCG, a Family of Better Random Number Generators](http://www.pcg-random.org/): en serie med bättre algoritmer för att generera slumpmässiga tal.
- [https://clojuredocs.org/clojure.core/rand](https://clojuredocs.org/clojure.core/rand): Officiell Clojure's dokumentation för `rand` funktionen.
- [https://clojuredocs.org/clojure.core/rand-int](https://clojuredocs.org/clojure.core/rand-int): Officiell Clojure's dokumentation för `rand-int` funktionen.