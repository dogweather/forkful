---
title:                "Gleam: Söka och ersätta text"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Har du någonsin behövt söka och ersätta text i ett stort dokument eller kodprojekt? Istället för att manuellt gå igenom varje instans av texten, kan du enkelt använda Gleams sök- och ersättningsfunktion för att snabbt och effektivt göra ändringar.

## Hur gör man

För att söka och ersätta text i Gleam, använder du funktionen `replace`. Till exempel, om du vill ersätta alla förekomster av "hej" med "hejsan", så skulle koden se ut så här:

```Gleam
let text = "Hej världen! Hej igen!"
let nyText = replace(text, "hej", "hejsan")
```

I detta fall kommer `nyText` att ha värdet "Hejsan världen! Hejsan igen!".

Det är också möjligt att söka och ersätta text med hjälp av reguljära uttryck. Istället för en sträng som sökfält, kan du använda ett reguljärt uttryck genom att lägga till `~r` före sökvärdet. Till exempel, om du vill ersätta alla siffror med "X", så skulle koden se ut så här:

```Gleam
let text = "Det är 123 text"
let nyText = replace(text, ~r`\d`, "X")
```

Nu kommer `nyText` att ha värdet "Det är XXX text"

## Djupdykning

Det finns flera saker att tänka på när du använder Gleams sök- och ersättningsfunktion. Först och främst är det viktigt att förstå skillnaden mellan att söka och ersätta i en ren sträng eller med ett reguljärt uttryck. Om du bara behöver söka och ersätta en specifik sträng, är det enklare att använda den första metoden som beskrivs ovan. Om du däremot behöver söka och ersätta mer komplext innehåll, såsom nummer eller bokstäver i ett visst mönster, är det bättre att använda reguljära uttryck.

Det är också viktigt att notera att Gleam är ett funktionellt programmeringsspråk, vilket innebär att `replace` inte kommer att modifiera den ursprungliga texten. Istället kommer den att returnera en ny värde med den sökta och ersatta texten.

## Se även

- [Gleams officiella hemsida](https://gleam.run/)
- [Dokumentation för sök- och ersättningsfunktionen i Gleam](https://gleam.run/documentation/search.html)