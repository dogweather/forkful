---
title:                "Arduino: Skapa en tillfällig fil"
simple_title:         "Skapa en tillfällig fil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa temporära filer är en vanlig praxis inom programmering för att tillfälligt lagra data eller utföra en speciell uppgift. Det är också användbart när du vill undvika att permanent ändra data på ett lagringsmedium. Med hjälp av temporära filer kan du enkelt spara och hantera data i ditt Arduino-program.

## Hur man skapar en temporär fil

Att skapa en temporär fil i ditt Arduino-program är relativt enkelt och kan göras med hjälp av standardfunktionen "tempFile()" i Arduino File System-biblioteket. Här är ett exempel på kod som skapar en temporär fil, skriver data till den och sedan stänger filen:

```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  // Initierar SD-kortet
  if (!SD.begin(4)) {
    Serial.println("Starta SD-kortet misslyckades!");
    return;
  }

  // Skapar en temporär fil
  File tempFile = SD.open("temp.txt", FILE_WRITE);

  // Skriver data till filen
  tempFile.println("Det här är en temporär fil.");
  tempFile.println("Används för att lagra data temporärt.");
  tempFile.close();  
}

void loop() {
  //tom loop
}
```

Efter att koden har körts kommer en temporär fil med namnet "temp.txt" att skapas på ditt SD-kort. Den här filen kommer innehålla de två rader som skrevs i koden ovan. Sedan kommer filen att stängas igen.

## Djupdykning

Förutom att bara skapa och använda temporära filer, finns det också andra sätt att hantera dem i ditt Arduino-program. Du kan till exempel använda funktionen "exists()" för att kontrollera om en temporär fil redan finns på ditt SD-kort innan du skapar en ny. Du kan också använda funktionerna "open()" och "remove()" för att öppna och ta bort temporära filer. Det finns även möjlighet att ange vissa parametrar för att skapa temporära filer med olika egenskaper, till exempel att de ska vara lästa och skrivna parallellt eller endast läsbara.

## Se även

Här är några användbara länkar för att lära dig mer om hur man skapar och hanterar temporära filer i Arduino:

- [Arduino File System-biblioteket](https://www.arduino.cc/en/Reference/File)
- [Hantering av temporära filer i Linux](https://www.redhat.com/sysadmin/temporary-file-management-linux)
- [Guide för temporära filer i C++](https://www.bogotobogo.com/cplusplus/temporaryfile.php)