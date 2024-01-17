---
title:                "Generera slumpmässiga nummer"
html_title:           "Clojure: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga tal handlar om att skapa nummer utan en fördefinierad ordning eller mönster. Programmörer använder detta ofta för att skapa variation och överraskningar i sina program eller för att testa funktioner som måste hantera olika typer av data.

## Så här:
Att generera slumpmässiga tal i Clojure är enkelt. Använd funktionen ```(rand)``` för att få ett slumpmässigt tal mellan 0 och 1, till exempel ```(rand) ; => 0.423971193303735```. Om du vill ha ett heltal, använd ```(rand-int n)``` där n är det högsta talet du vill ha, till exempel ```(rand-int 10) ; => 5```.

## Fördjupning:
Att generera slumpmässiga tal har varit en viktig del av datavetenskap sedan dess början på 1940-talet. Det finns många olika algoritmer och metoder för att generera slumpmässiga tal, men vissa kan vara bättre än andra beroende på vilket syfte de används för. Om du inte är nöjd med Clojures inbyggda funktioner för att generera slumpmässiga tal kan du prova att använda en separat bibliotek som ```random``` eller skapa din egen egendefinierade algoritm.

## Se även:
- Random modulen i Clojure: https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/rand
- Random biblioteket: https://github.com/reiddraper/random
- Wikipedia sida om generering av slumpmässiga tal: https://en.wikipedia.org/wiki/Random_number_generation