---
title:                "Läsning av en textfil"
html_title:           "Arduino: Läsning av en textfil"
simple_title:         "Läsning av en textfil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa en textfil innebär att läsa information från en textfil och använda den i ditt Arduino-program. Programmörer gör det för att kunna använda viktiga data eller konfigurationsinställningar som är lagrade i en extern textfil istället för att hårdkoda dem i koden.

## Hur man gör:
För att läsa en textfil i Arduino använder vi "File" biblioteket och dess funktioner. Först måste vi öppna textfilen som vi vill läsa med hjälp av `File.open()`. Sedan kan vi använda funktionen `File.read()` för att läsa in en enskild bokstav och `File.readString()` för att läsa in hela textfilen som en sträng. Nedan är ett exempel på hur man kan läsa en textfil:
```
Arduino ... 
// Öppna textfilen
File file = File.open("exempel.txt", FILE_READ);

// Läsa in första bokstaven i filen
char c = file.read();

// Läsa in hela textfilen som en sträng
String str = file.readString();

// Stäng textfilen
file.close();
```
Output:
```
c = 'H'
str = "Hej! Det här är en textfil."
```

## Djupdykning:
Att kunna läsa en textfil är en viktig funktion för att kunna göra mer avancerade och dynamiska program på Arduino. Tidigare var det vanligt att hela programkoden var hårdkodad, men nu används ofta externa filer för att kunna ändra och uppdatera information utan att behöva ändra koden. Alternativ till att läsa textfiler i Arduino är att använda EEPROM eller att kommunicera med en annan enhet via till exempel Serial kommunikation.

För att implementera läsning av textfiler i ditt program, se till att ha rätt format på textfilen (till exempel .txt eller .csv) och använd `File.open()` funktionen bör vara en av de första funktionerna som anropas i `setup()` delen av ditt program. Notera också att Arduino bara kan läsa textfiler som finns på det interna eller externa minnet på din enhet.

## Se även:
[Hur man skriver en textfil i Arduino](https://www.arduino.cc/en/Reference/FileWrite)

[Arduino "File" bibliotek](https://www.arduino.cc/reference/en/libraries/filesystem/file/)