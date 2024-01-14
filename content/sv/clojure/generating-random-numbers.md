---
title:                "Clojure: Generera slumpmässiga nummer"
programming_language: "Clojure"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför
Att generera slumpmässiga nummer är en avgörande del av datavetenskap och programmering. Det möjliggör skapande av olika användbarheter såsom simuleringar, spel och kryptografi.

## Hur man gör
För att generera slumpmässiga nummer i Clojure, kan vi använda oss av inbyggda funktioner som "rand" och "rand-int". Dessa funktioner tar argument och producerar ett slumpmässigt nummer med hjälp av en pseudorandom generator.

```Clojure
(rand) ; => 0.572945789935983
(rand) ; => 0.136297439679616
```

Vi kan också begränsa intervallet av slumpmässiga nummer genom att ange ett argument till funktionen "rand-int":

```Clojure
(rand-int 10) ; => 5
(rand-int 50) ; => 22
```

Slutligen kan vi använda funktionen "repeatedly" för att generera ett specifikt antal slumpmässiga nummer:

```Clojure
(repeatedly 5 rand-int) ; => (3 1 4 7 9)
(repeatedly 10 #(rand-int 100)) ; => (26 78 4 65 89 15 90 31 98 53)
```

## Djupdykning
Slumpmässiga nummer som genereras av "rand" och "rand-int" är inte riktigt slumpartade i matematisk mening. De skapas med hjälp av en så kallad pseudorandom generator, vilket innebär att de är baserade på ett startvärde och sedan använder en algoritm för att generera serier av nummer som verkar slumpmässiga.

Clojure erbjuder också ett sätt att använda en egendefinierad generator genom funktionen "set!" och "seed-random". Detta kan vara användbart om du vill ha mer kontroll över dina slumpmässiga nummer.

## Se även
- [ClojureDocs: random](https://clojuredocs.org/clojure.core/rand)
- [ClojureDocs: repeated](https://clojuredocs.org/clojure.core/repeatedly)
- [Wikipedia: Pseudorandom generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)