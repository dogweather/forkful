---
title:                "Läsa en textfil"
html_title:           "Fish Shell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa en textfil är processen där vi hämtar data från en fil och tolkar det inom vår programvara. Programmerare gör detta för att spara, analysera och återanvända data utan att behöva skriva alltihop manuellt i kod.

## Så här gör du:
Med Arduino, kan du läsa en textfil från ett SD-kort med några enkla kodbittar. För att göra detta, behöver du en SD-kortsläsare och Arduino SD-biblioteket. 

```Arduino
#include <SPI.h>
#include <SD.h>

void setup()
{
  Serial.begin(9600);
  while (!Serial) {;}
  
  if (!SD.begin(4)) {
    Serial.println("Kortets initialisering misslyckades!");
    return;
  }
  File myFile = SD.open("test.txt");
  
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  }
}
void loop()
{
  // nothing happens after setup
}
```
Denna kod läser en fil som heter "test.txt" från ett SD-kort.

## Fördjupning
Historiskt sett använde tidiga programmerare textfiler för att lagra och byta information mellan olika system. Trots framväxten av databaser och API:er, är textfiler fortfarande en greppbar och enkel lösning för att spara data.

Alternativen till att läsa en textfil inkluderar att använda en databas eller ett API. Dessa alternativ kan vara mer kraftfulla och skalbara, men också mer komplicerade att använda och inte nödvändigtvis lämpliga för alla projekt.

När det gäller verkställande detaljer, utför Arduino "myFile.read()" funktionen för att läsa en byte av data från filen, och "myFile.available()" för att avgöra om det finns mer data att läsa.

## Se även
Kolla in dessa länkar för mer information:
- [Arduino SD Biblioteket](https://www.arduino.cc/en/reference/SD) 
- [SPI Kommunikation](https://www.arduino.cc/en/reference/SPI)
- [Läsning av SD-kort](https://learn.adafruit.com/adafruit-micro-sd-breakout-board-card-tutorial)