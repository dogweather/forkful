---
title:    "Gleam: Sökning och ersättning av text"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text kan kännas som en enkel uppgift, men det kan spara dig mycket tid och möda när du jobbar med mycket kod. Med Gleam har du möjlighet att snabbt och enkelt söka igenom stora mängder text och ersätta den med önskad text, vilket gör det till ett ovärderligt verktyg för alla kodbaser.

## Så här gör du

För att använda Gleams funktion för att söka och ersätta text, behöver du först importera den inbyggda modulen "String.Replace". Därefter kan du använda funktionen "replace" som tar tre parametrar: en sträng att söka igenom, den text du vill ersätta, och den text du vill ersätta den med. Koden nedan visar ett enkelt exempel på hur du söker igenom en sträng och ersätter all förekomst av ordet "hej" med "hallå":

```Gleam
import String.Replace

let str = "Hej världen, hej alla!"

let result = String.Replace.replace(str, "hej", "hallå")

gör result // "Hallå världen, hallå alla!"
```

Som du kan se är det enkelt och snabbt att söka och ersätta text med hjälp av Gleam.

## Djupdykning

När du använder funktionen "replace" finns det några parametrar du kan lägga till för att anpassa din sökning och ersättning. Den första parametern är "case_sensitive" som bestämmer om sökningen endast ska leta efter exakta matchningar av den sökta texten, eller om den också ska matcha med olika bokstavskombinationer. Standardvärdet för denna är "false" vilket betyder att det inte spelar någon roll om du skriver "hej" eller "Hej", dessa kommer att matcha båda två.

En annan användbar parameter är "global" som bestämmer om åtgärden endast ska utföras på den första matchningen eller på alla matchningar. Om du till exempel vill ersätta alla förekomster av "hej" i en text med "hallå" kan du ställa in denna parameter till "true".

Det finns också en tredje parameter som heter "limit" som bestämmer hur många matchningar som ska ersättas. Om du bara vill ersätta de två första förekomsterna av "hej" i en text kan du ställa in denna parameter till "2".

Med dessa parametrar kan du anpassa sökningen och ersättningen för att passa dina specifika behov.

## Se även

- Gleam Officiell Dokumentation: https://gleam.run/
- "Strings" modulen: https://gleam.run/modules/string/
- "String.Replace" modulen: https://gleam.run/modules/string-replace/