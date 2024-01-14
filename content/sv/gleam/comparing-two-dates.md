---
title:                "Gleam: Jämförelse av två datum"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en vanlig uppgift inom programmering. Det är användbart för att kontrollera om ett datum ligger före eller efter ett annat, eller om de två datumen är lika. Oavsett vilken typ av program du utvecklar, är det viktigt att ha en bra förståelse för hur man gör detta.

## Hur man gör 

```Gleam
Date.comparison(Date.new(2021, 01, 01), Date.new(2020, 12, 31))
```

Detta kodblock visar hur man använder den inbyggda funktionen "comparison" i Gleam för att jämföra två datum. Funktionen tar två datum som argument och returnerar ett heltal som indikerar hur de förhåller sig till varandra. Om det första datumet är tidigare än det andra kommer resultatet att vara -1, om det kommer efteråt blir det 1 och om de båda är samma kommer det att vara 0.

## Deep Dive

Att jämföra datum kan verka enkelt, men det finns vissa saker att tänka på för att undvika fel. En viktig aspekt är att se till att båda datumen har samma format, annars kan jämförelsen ge ett felaktigt resultat.

En annan viktig detalj är att datumet måste konverteras till GMT-tid innan det kan jämföras, eftersom olika tidszoner och sommartider kan påverka resultatet. Detta kan åstadkommas genom att använda funktionen "DateTime.to_gmt".

## Se även

- Gleams inbyggda datumfunktioner: https://gleam.run/docs/std/datetime/
- En guide om hur man jämför datum i andra programmeringsspråk: https://www.w3schools.com/sql/func_sqlserver_formats.asp