---
title:                "Arbeta med CSV"
html_title:           "Arduino: Arbeta med CSV"
simple_title:         "Arbeta med CSV"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

##Varför

CSV-filer är vanligt förekommande i datahantering och används ofta för att lagra stora mängder information på ett enkelt och lättläst sätt. Genom att lära sig arbeta med CSV-filer kan du effektivt samla och bearbeta data för dina Arduino-projekt.

##Så här gör du

För att kunna arbeta med CSV-filer på din Arduino, behöver du först och främst lägga till ett bibliotek för CSV-hantering till ditt projekt. Ett populärt sådant bibliotek är "ArduinoCSV", som du kan ladda ner och installera via "Sketch" > "Include Library" > "Manage Libraries" i Arduino IDE.

När biblioteket är installerat kan du börja arbeta med CSV-filer genom att skapa en ny variabel av typen "CSV" och ange sökvägen till din CSV-fil som argument. Sedan kan du använda funktioner som "readCSV" för att läsa in data från filen och "writeCSV" för att skriva data till filen.

Här är ett exempel på kod för att läsa in data från en CSV-fil och skriva ut den på seriell port:

```
#include <ArduinoCSV.h>                     // Hämtar biblioteket

CSV myFile("data.csv");                     // Skapar en variabel för vår fil
if(myFile.open()){                         // Öppnar filen
  while(myFile.available()){                // Loopar tills all data är läst
    Serial.print(myFile.read());           // Skriver ut data på seriell port
  }
  myFile.close();                           // Stänger filen
}
```

##Djupdykning

En CSV-fil (Comma-Separated Values) består av rader med data, där varje rad är uppdelad i flera kolumner genom kommatecken. Det finns olika sätt att hantera CSV-filer beroende på deras struktur och datainnehåll.

Om filen innehåller en rad med kolumnrubriker, kan du använda funktionen "readHeader" för att läsa in dessa rubriker och sedan använda dem som referens för att läsa in data från relevanta kolumner. Du kan också använda funktionen "writeHeader" för att skriva in kolumnrubriker i en ny fil.

Om du vill lägga till data i en befintlig CSV-fil, kan du använda funktionen "append" istället för "read" för att lägga till data efter den sista raden i filen.

Det finns också möjlighet att arbeta med mer avancerade CSV-format, som delade kolumner eller olika tecken för gruppering av data. Detta kräver dock en mer omfattande förståelse för CSV-specifikationen och eventuellt anpassade funktioner.

##Se även

- [Arduino Libraries](https://www.arduino.cc/en/Reference/Libraries)
- [ArduinoCSV GitHub Repository](https://github.com/adafruit/Adafruit_CircuitPython_CSV)
- [CSV Specifications](https://tools.ietf.org/html/rfc4180)