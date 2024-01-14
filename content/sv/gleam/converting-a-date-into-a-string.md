---
title:                "Gleam: Omvandling av ett datum till en sträng"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera en datum till en sträng är en vanlig uppgift inom programmering, särskilt när man arbetar med datum och tidsuppgifter. Detta kan vara användbart för att presentera datumdata i ett läsbart format eller för att jämföra datumvärden.

## Hur man gör det
För att konvertera ett datum till en sträng i Gleam, behöver vi använda funktionen `DateTime.to_string()`. Vi kan också ange ett visst format för datumet om vi vill, till exempel `DateTime.to_string("{YYYY}-{MM}-{DD}")`. Nedan följer ett exempel på hur man kan konvertera ett datum till en sträng i Gleam:

```Gleam
let date = DateTime.local(2021, 10, 31)
let date_str = DateTime.to_string(date, "{DD}/{MM}/{YYYY}")
```

I detta exempel skapar vi ett datumobjekt som representerar den 31 oktober 2021, och konverterar det sedan till en sträng som följer formatet dag/månad/år.

När vi kör koden ovan kommer variabeln `date_str` att ha värdet "31/10/2021".

## Djupdykning
När vi konverterar ett datum till en sträng bör vi vara medvetna om den faktiska representationen av datumet. I Gleam representeras datumet i ISO 8601-format, vilket innebär att år, månad och dag följer efter varandra. Detta kan se lite annorlunda ut jämfört med traditionell formatering, där månad och dag ofta är ombytta.

En annan viktig aspekt att komma ihåg vid konverteringen är att vi måste välja en korrekt tidzon för det specifika datumet. I Gleam används en standardtidszon om vi inte anger en specifik tidzon.

För att lära dig mer om datumrepresentation och funktioner för datumkonvertering i Gleam, rekommenderar vi att du läser dokumentationen för DateTime-modulen.

## Se även
- [DateTime-modulen i Gleam dokumentation](https://gleam.run/school/datetime)
- [ISO 8601-standarden för datumrepresentation](https://sv.wikipedia.org/wiki/ISO_8601)