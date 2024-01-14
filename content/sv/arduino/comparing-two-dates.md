---
title:                "Arduino: Jämförande av två datum"
simple_title:         "Jämförande av två datum"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum kan vara användbart i många olika situationer, till exempel för att se hur lång tid som gått mellan två händelser eller för att beräkna ålder. Med Arduino-programmering har du möjlighet att enkelt utföra sådana jämförelser och få ut relevant information.

## Hur man gör

För att jämföra två datum i Arduino behöver du först definiera de två datum som ska jämföras. Detta görs genom att skapa två "Date" objekt som representerar de två olika tidpunkterna. Till exempel:

```Arduino 
Date tidpunkt1 = Date(2020, 1, 1);
Date tidpunkt2 = Date(2021, 5, 20);
```

För att sedan jämföra dessa två datum finns det flera olika inbyggda funktioner att använda sig av. Här är ett exempel på hur man kan jämföra om det första datumet är efter det andra:

```Arduino 
if (tidpunkt1 > tidpunkt2) {
    Serial.println("Tidpunkt 1 är senare än tidpunkt 2");
}
```

Du kan också jämföra om det första datumet är lika med det andra, eller om det är före det andra datumet. Det finns även möjlighet att jämföra exakt tidpunkter, till exempel om de båda inträffar på samma dag, månad och år.

## Djupdykning

När man jämför två datum är det viktigt att komma ihåg att en dag egentligen är ett antal sekunder (eller millisekunder) som har passerat sedan en viss starttidpunkt, vanligtvis 1 januari 1970. Detta kallas för en "epoch" och är en standardiserad tidpunkt som används inom många olika programmeringsspråk.

När du definierar dina "Date" objekt i Arduino så kan du välja om du vill ange tiden i millisekunder eller i sekunder. Detta kan vara viktigt att tänka på om du jämför datum från olika källor, eftersom en millisekund kan göra skillnad i en jämförelse.

## Se även

- [Date referens](https://www.arduino.cc/reference/en/language/functions/time/date/)
- [Epoch-tid](https://en.wikipedia.org/wiki/Unix_time)