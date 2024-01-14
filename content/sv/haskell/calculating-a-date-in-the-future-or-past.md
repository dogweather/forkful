---
title:    "Haskell: Beräkning av datum i framtiden eller förr"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Varför

Att kunna räkna ut en datum i framtiden eller det förflutna är en viktig funktion inom programmering. Det gör det möjligt att planera och hantera datum och tidsberäkningar på ett enkelt och effektivt sätt.

## Så här gör du

För att räkna ut ett datum i framtiden eller det förflutna i Haskell, behöver du använda funktionen `addDays` tillsammans med ett `Day`-objekt. Här är ett exempel där vi vill beräkna datumet 14 dagar framåt från idag:

```Haskell
let today = fromGregorian 2020 6 1
let futureDate = addDays 14 today
print futureDate
```

Detta skulle ge oss output: `2020-06-15`. Om vi istället vill beräkna datumet 7 dagar bakåt från idag, skulle koden se ut så här:

```Haskell
let today = fromGregorian 2020 6 1
let pastDate = addDays (-7) today
print pastDate
```

Outputen skulle då bli: `2020-05-25`. Det är viktigt att notera att funktionen `fromGregorian` används för att skapa ett `Day`-objekt utifrån år, månad och dag. Detta är viktigt eftersom `addDays` endast kan användas tillsammans med `Day`-objekt.

## Djupdykning

Om vi vill göra mer avancerade beräkningar, såsom att lägga till eller dra bort månader eller år från ett datum, kan vi använda funktionen `addGregorianMonthsClip` tillsammans med `addGregorianYearsClip`. Dessa funktioner tar in samma parametrar som `addDays`, men är specifikt utformade för att beräkna månader och år. Här är ett exempel där vi vill räkna ut datumet 6 månader framåt från idag:

```Haskell
let today = fromGregorian 2020 6 1
let futureDate = addGregorianMonthsClip 6 today
print futureDate
```

Output: `2020-12-01`.

För mer information om dessa funktioner och andra som används för att hantera datum i Haskell, rekommenderar jag att du tar en titt på [Haskell Dokumentationen om Datum och Tid](https://hackage.haskell.org/package/time-1.10.0.1/docs/Data-Time-Calendar.html).

## Se även

- [Haskell Dokumentationen om Datum och Tid](https://hackage.haskell.org/package/time-1.10.0.1/docs/Data-Time-Calendar.html)
- [Stack Overflow diskussion om beräkning av datum i Haskell](https://stackoverflow.com/questions/40281265/calculating-future-date-in-a-haskell-program)