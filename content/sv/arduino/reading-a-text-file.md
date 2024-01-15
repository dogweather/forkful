---
title:                "Läsa en textfil"
html_title:           "Arduino: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför läsa en textfil?

Att läsa en textfil är en grundläggande del av programmering och kan hjälpa till att spara tid och öka effektiviteten. Genom att läsa en textfil kan du enkelt hämta data som har sparats i form av text och använda det i dina program.

## Så här gör du

Att läsa en textfil i Arduino är en enkel process. Först måste du skapa en textfil på datorn med den data som du vill använda. Sedan kan du följa kodexemplen nedan för att läsa filen och skriva ut dess innehåll på serieporten.

```Arduino
// Öppna textfilen
File file = SD.open("namn_pa_textfil.txt");

// Läs filen rad för rad
while (file.available()) {
  Serial.println(file.readStringUntil('\n'));
}

// Stäng filen
file.close();
```

Resultatet av koden ovan kommer att skriva ut innehållet i filen på serieporten, där varje rad representeras av en rad i serieporten.

## Fördjupning

Det finns flera saker att tänka på när du läser en textfil i Arduino. En av de viktigaste är att se till att filen är korrekt formaterad med rätt teckenkodning och radavslutare. Det är också viktigt att vara medveten om storleken på filen och utrymmet som den tar upp på ditt Arduino-minne.

## Se även

- [Arduino SD Library Reference](https://www.arduino.cc/en/Reference/SD) - Referens för att använda SD-kort med Arduino
- [ASCII-teckenkodning](https://sv.wikipedia.org/wiki/ASCII) - Förståelse för ASCII-kodning och dess användning i textfiler
- [Radavslutare](https://sv.wikipedia.org/wiki/Radavslutare) - Översikt över olika typer av radavslutare och hur de används i olika system