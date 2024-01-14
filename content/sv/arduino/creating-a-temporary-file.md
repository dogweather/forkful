---
title:    "Arduino: Skapa en temporär fil"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa temporära filer är en viktig del av Arduino-programmering eftersom det ger dig möjlighet att lagra data på ett flexibelt sätt. Genom att använda temporära filer kan du tillfälligt spara data som du kan använda i ditt Arduino-program.

## Så här gör du

För att skapa en temporär fil behöver du följa några enkla steg:

1. Skapa en variabel för din temporära fil och ge den ett namn.
2. Öppna filen med "File.open" kommandot och ange vad filen ska heter.
3. Skriv in den data som ska sparas i filen med hjälp av "File.print" eller "File.println" kommandon.
4. Stäng filen med "File.close" kommandot för att säkerställa att all data har sparats korrekt.

Här är ett exempel på hur koden kan se ut i ett Arduino-program:

```Arduino
File tempFile; // skapar variabel för temporär fil

void setup() {
  Serial.begin(9600);
  tempFile = SPIFFS.open("/temp.txt", "w"); // öppnar filen för skrivning
  tempFile.print("Arduino är en fantastisk mikrokontroller!"); // skriver data i filen
  tempFile.close(); // stänger filen
}

void loop() {
  // kod för ditt program
}
```

När du laddar detta program på din Arduino kommer en temporär fil med namnet "temp.txt" att skapas och innehålla texten "Arduino är en fantastisk mikrokontroller!". Du kan sedan använda denna fil för att hämta och använda data i ditt program.

## Djupdykning

Att skapa temporära filer är också användbart när du behöver spara information som bara är relevant för en kort period av ditt program. Det kan hjälpa till att spara minnesutrymme och göra din kod mer effektiv.

Du kan också använda temporära filer för att skapa loggar eller spara sensorvärden som sedan kan användas för att analysera dina projekt.

## Se även

Här är några länkar som kan vara användbara för att lära dig mer om att skapa temporära filer i Arduino:

- [Arduino Fil Referens](https://www.arduino.cc/en/Reference/File)
- [Sparkfun guide till SPIFFS för ESP8266](https://learn.sparkfun.com/tutorials/spiffs---using-the-littlefs-arduino-library/all)
- [Video tutorial om att spara data på SPIFFS med ESP32](https://www.youtube.com/watch?v=fNkGKqJ2LMs)