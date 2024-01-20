---
title:                "Arbeta med csv"
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV, eller comma-separated values, är ett enkelt filformat för att lagra och dela data i tabellform. Det används ofta av programmerare eftersom det är ett universellt format som är lätt att läsa och skriva för både människor och datorer.

## Så här gör du:
För att läsa och skriva CSV-filer med Arduino, behöver du använda dig av ett bibliotek som heter "ArduinoCSV". För att installera detta bibliotek, gå till Arduinos "Sketch" -> "Include Library" -> "Library Manager" och sök efter "ArduinoCSV". När det är installerat, kan du använda kommandon som "read_CSV()" och "write_CSV()" för att läsa och skriva CSV-filer från en SD-kortmodul eller från serieporten.

```Arduino
#include <ArduinoCSV.h> // Inkluderar biblioteket

void setup() {
  Serial.begin(9600);
}

void loop() {
  CSV data; // Skapar en variabel för att lagra CSV-data
  if (data.read_CSV("data.csv")){ // Läser CSV-filen "data.csv"
    for (int i = 0; i < data.rows; i++) {
      Serial.print(data[i][0]); // Skriver ut första kolumnen
      Serial.print(" : ");
      Serial.print(data[i][1]); // Skriver ut andra kolumnen
      Serial.println(); // Radbrytning
    }
  }
}

//Output:
/*
1 : John
2 : Jane
3 : Bob
*/
```

## Djupdykning:
CSV-filer har funnits sedan 1972 och är ett av de mest använda filformaten i världen. Alternativ till CSV inkluderar JSON och XML, men CSV är mer lättläst och behöver mindre lagringsutrymme. Implementeringsdetaljer inkluderar att man måste ange tecknet som separerar kolumnerna (default är kommatecken) och att man måste hantera tecken som kan störa som citattecken eller radbrytningar.

## Se även:
- [CSV-filer](https://en.wikipedia.org/wiki/Comma-separated_values)