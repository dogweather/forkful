---
title:                "Skriva en textfil"
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför 

Att kunna skriva en textfil kan vara användbart i många olika projekt med Arduino. Det kan till exempel användas för att spara data eller text som sedan kan användas för att styra andra delar av ditt projekt.

## Hur man gör det 

Det första steget för att skriva en textfil på Arduino är att öppna en seriell anslutning genom att använda "Serial.begin ()" kommandot. Sedan kan du använda "SD.begin ()" kommandot för att aktivera SD-kortet som kommer att användas för att lagra textfilen. Du kan specificera namnet på filen med hjälp av "open ()" och sedan använda "write ()" för att faktiskt skriva in texten i filen.

 ```Arduino
Serial.begin(9600);
SD.begin(4); // Pin 4 som anslutning till SD-kortet

File myFile = SD.open("myfile.txt", FILE_WRITE); // Skapar en fil med namnet "myfile.txt" och öppnar den för skrivning

if (myFile) { // Om filen har öppnats korrekt
  myFile.println("Hej, detta är min textfil!"); // Skriver in "Hej, detta är min textfil!" i filen
  myFile.close(); // Stänger filen
}
```

Om allt gått rätt till kommer texten att skrivas in i filen och du kan läsa den från SD-kortet genom att öppna filen på din dator. Om du vill skriva till en befintlig fil används "append ()" istället för "open ()". Detta gör att din nya text läggs till i slutet av filen istället för att skriva över den befintliga texten.

## Djupdykning 

För att skriva en textfil på Arduino behöver du ett SD-kortläsaremodul som ansluts till din Arduino med hjälp av SPI-kommunikation. Du behöver också ett SD-kort som är formaterat till FAT16 eller FAT32. FAT står för File Allocation Table och är ett filsystem som används för att organisera data på SD-kortet.

När du skapar en ny fil med "open ()" måste du ange om filen ska öppnas för skrivning, läsning eller båda två. Detta görs genom att inkludera "FILE_WRITE", "FILE_READ" eller "FILE_WRITE|FILE_READ" som en parameter i "open ()". Om du inte inkluderar någon av dessa parametrar kommer filen att öppnas för läsning som standard.

## Se även 

- [Arduino SD Library Reference](https://www.arduino.cc/en/Reference/SD)
- [SD Card Module Tutorial](https://create.arduino.cc/projecthub/JonathanJankelowicz/sd-card-module-tutorial-how-to-add-more-memory-to-your-arduino-4b3264)