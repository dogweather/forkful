---
title:                "Swift: Radera tecken som matchar ett mönster"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Vi är alla bekanta med det vanliga problemet att behöva ta bort specifika tecken från en sträng. Oavsett om det är mellanslag, punkter eller återkommande tecken, kan det vara en utmaning att göra det manuellt. Men oroa dig inte, Swift har en lösning för detta problem - ta bort tecken som matchar ett mönster.

## Varför

Att ta bort tecken som matchar ett mönster kan hjälpa till att rensa upp en sträng och göra den mer läsbar. Det kan också vara användbart när man arbetar med användarinmatningar, där vissa tecken kanske behöver tas bort för att upprätthålla dataintegriteten.

## Så här gör du

För att ta bort tecken som matchar ett mönster i Swift, behöver vi använda oss av en inbyggd funktion som heter `removingAllMatching`. Den tar ett regex-mönster och returnerar en ny sträng utan de matchande tecknen.

Låt oss anta att vi vill ta bort alla siffror från en sträng. Vi kan använda detta regex-mönster: `[0-9]`. I Swift skulle det se ut så här:

```Swift
let sträng = "Detta är en 1 test 2 sträng 3 utan 4 siffror."
låt nyString = string.replacingAllMatching ("[0-9]", med: "")
print (nyString) // Output: "Detta är en test sträng utan siffror."
```

Som du kan se tar `removingAllMatching`-funktionen bort alla siffror från vår ursprungliga sträng och returnerar en ny sträng utan dem.

## Djupdykning

I exemplet ovan använde vi ett ganska enkelt regex-mönster för att ta bort siffror. Men det finns många olika sätt att utforma mönster för att matcha specifika tecken eller teckengrupper. En mer avancerad användning av `removingAllMatching`-funktionen skulle vara att ta bort alla specialtecken och bara behålla bokstäver.

Här är ett exempel på ett regex-mönster som kan användas för detta ändamål: `[^a-zåäöA-ZÅÄÖ]`. Detta mönster matchar alla tecken som inte är bokstäver i det svenska alfabetet. Så i vår funktion skulle det se ut så här:

```Swift
let sträng = "Detta är en teststräng med %^specialtecken."
låt nyString = string.replacingAllMatching ("[^a-zåäöA-ZÅÄÖ]", med: "")
print (nyString) // Output: "Detta är en teststräng med särskiljande."
```

Som du kan se tar vår funktion nu bort alla specialtecken och endast behåller bokstäverna i strängen.

## Se även

- [Apple Dokumentation om strings och teckensättning](https://developer.apple.com/documentation/foundation/nsstring)
- [Regexp Gudie för Swift](https://github.com/sharplet/Regex)

Tack för att du läste! Vi hoppas att denna guide har varit hjälpsam i att lära dig hur du enkelt kan ta bort tecken som matchar ett mönster i Swift. Ha det så kul med att koda!