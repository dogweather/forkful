---
title:                "Arduino: Att arbeta med csv"
simple_title:         "Att arbeta med csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

CSV är ett vanligt filformat för att lagra och dela data. Att lära sig hur man läser och behandlar CSV-filer i Arduino kan öppna upp möjligheter för att integrera externa datakällor eller skapa program som behöver hantera stora mängder data.

## Hur man gör

För att läsa och behandla CSV-filer i Arduino behöver du först importera ett specialbyggt bibliotek som heter "CSV.h". Detta bibliotek ger dig funktioner för att öppna, läsa och bearbeta CSV-filer.

För att öppna en CSV-fil använder du funktionen "parse". Exempelkod nedan visar hur du kan använda denna funktion för att öppna en fil vid namn "data.csv" på ett SD-kort:

```
#include <SD.h> // importera SD biblioteket
#include <CSV.h> // importera CSV biblioteket
File dataFile;

void setup() {
  Serial.begin(9600);
  // montera SD-kortet
  if (!SD.begin(10)) {
    Serial.println("Error: SD-kort kunde inte monteras.");
    return;
  }
  // öppna CSV-filen
  dataFile = SD.open("data.csv");
  // skicka filen till funktionen "parse"
  CSV.parse(&dataFile);
}

void loop() {
  // hämta en rad från filen
  Row rowData = CSV.readRow();
  // skriv ut innehållet i raden
  Serial.println(rowData[0]); // skriver ut första värdet i raden
}
```

Output av ovanstående kod kommer att varje rad i CSV-filen, med det första värdet i raden skrivet ut till seriell monitor.

## Deep Dive

För att kunna behandla data på ett effektivt sätt rekommenderas det att du konverterar den lästa datan till lämpliga datatyper. Till exempel, om du vet att det första värdet i raden är en float, kan du använda "toFloat" funktionen för att konvertera det lästa värdet till en float.

Det är även viktigt att hantera eventuella separatorer (vanligtvis komma eller semikolon) mellan värdena i raden. Funktionen "setDelimiter" kan användas för att specificera vilken separator som används i filen.

För mer avancerade användare finns det även möjlighet att ändra och spara data till CSV-filer med hjälp av funktionerna "write" och "save". Dessa är användbara om du behöver uppdatera eller skapa nya CSV-filer.

## Se också

- [CSV bibliotek för Arduino](https://github.com/rodolk/TinyCSV)
- [Tutorial för att läsa och behandla CSV-filer i Arduino](https://circuitdigest.com/microcontroller-projects/arduino-csv-file-processing)
- [Mer information om CSV-filer](https://en.wikipedia.org/wiki/Comma-separated_values)