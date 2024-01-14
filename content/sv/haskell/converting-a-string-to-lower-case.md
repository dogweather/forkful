---
title:                "Haskell: Omvandla en sträng till gemener"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener (lower case) är ett vanligt problem när man arbetar med textbehandling i Haskell. Genom att göra detta kan man åstadkomma konsistens och enhetlighet i ens data, vilket är viktigt för många applikationer.

## Hur man gör det

Att konvertera en sträng till gemener i Haskell är ganska enkelt. Det finns en inbyggd funktion som heter `toLower` som tar emot en sträng som argument och returnerar en ny sträng med alla bokstäver i gemener.

För att använda denna funktion behöver du importera modulen `Data.Char` genom att lägga till `import Data.Char` längst upp i din fil.

Här är ett exempel som visar hur man konverterar en sträng till gemener och sedan skriver ut resultatet:

```Haskell
import Data.Char

main = do
    let string = "HEJ ALLA SWEDISH READERS!"
    let lower = toLower string
    putStrLn lower
```

Kör detta och du borde få utskriften `hej alla swedish readers!`.

## Djupdykning

Nu när vi vet hur man konverterar en sträng till gemener låt oss ta en titt på vad som händer under huven.

`toLower` funktionen använder sig av den inbyggda funktionen `ord` för att konvertera en karaktär till motsvarande numeriska värde i ASCII-tabellen. Sedan använder den funktionen `chr` för att konvertera detta värde tillbaka till en gemener karaktär.

Det betyder att `toLower` inte bara fungerar med bokstäver i det engelska alfabetet, utan det fungerar också med specialtecken, accenter och alfabet från andra språk.

Om du vill gräva ännu djupare så kan du ta en titt på källkoden för `toLower` genom att köra kommandot `:browse Data.Char` i GHCI (Haskell interpreter).

## Se även

- [Haskell dokumentation för `Data.Char`](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [ASCII-tabell](https://www.td.unh.edu “ASCII-tabell”)