---
title:                "Haskell: Raderande av tecken som matchar ett mönster"
simple_title:         "Raderande av tecken som matchar ett mönster"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett mönster kan vara användbart när du vill filtrera eller rensa data i Haskell. Det kan också vara ett sätt att göra din kod mer läsbar och strukturerad. Genom att lära dig hur man tar bort tecken som matchar ett mönster kan du bli en mer effektiv programmerare.

## Så här gör du

Ett enkelt sätt att ta bort tecken som matchar ett mönster är genom att använda funktionen `filter`. Här är en kodexempel som tar bort alla tecken som matchar bokstaven "a" från en lista:

```Haskell
filter (\x -> x /= 'a') "Haskell är ett funktionellt programmeringsspråk"
```

Detta skulle resultera i följande utmatning: "Hskell r ett funktionellt progrmmereingssprlk".

Du kan också använda funktionen `delete` från paketet Data.List för att ta bort alla förekomster av ett visst tecken från en sträng. Till exempel:

```Haskell
delete 'a' "Haskell är ett funktionellt programmeringsspråk"
```

Detta skulle ge utmatningen "Hskell är ett funktionellt progrmmeringsspråk".

## Djupdykning

För att förstå hur funktionerna `filter` och `delete` fungerar, är det viktigt att förstå hur mönstermatchning fungerar i Haskell. När du säger `x /= 'a'` i koden ovan, anger du ett mönster som beskriver vilka tecken som ska matchas och tas bort. I detta fall betyder detta "alla tecken som inte är lika med 'a'".

Haskell har också stöd för reguljära uttryck, vilket kan vara användbart för mer avancerad mönstermatchning. Genom att använda reguljära uttryck kan du till exempel ta bort alla siffror eller specialtecken från en sträng.

## Se också

- [Data.List dokumentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html)
- [Haskell Regular Expressions-tutorial](https://wiki.haskell.org/Regular_expressions)