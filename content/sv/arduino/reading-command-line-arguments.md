---
title:                "Läsa kommandoradsargument"
html_title:           "Arduino: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att läsa kommandoradsargument är en vanlig praxis bland programmerare för att kunna anpassa en kod till olika användningsområden. Genom att läsa kommandoradsargument kan vi välja viss funktionalitet, ange specifika värden eller utföra olika uppgifter i en programvara.

## Så här gör du:

För att läsa kommandoradsargument i Arduino, använd funktionen `parseCommand()` tillsammans med `Serial.begin()` för att skicka kommandon från din dator till en ansluten Arduino-enhet. Se nedan för ett kodexempel:

```Arduino
int num; // Skapa en variabel för att lagra värdet på det första kommandot
float val; // Skapa en variabel för att lagra värdet på det andra kommandot

void setup() {
  Serial.begin(9600); // Starta seriell kommunikation med baudhastighet på 9600
}

void loop() {
  if (Serial.available()) {
    String command = Serial.readStringUntil('\n'); // Läs in det första kommandot och lagra det som en sträng
    num = parseInt(command); // Konvertera strängen till ett heltal och lagra det i variabeln num
    command = Serial.readStringUntil('\n'); // Läs in det andra kommandot och lagra det som en sträng
    val = parseFloat(command); // Konvertera strängen till ett decimaltal och lagra det i variabeln val
    // Här kan du utföra olika uppgifter beroende på de inlästa kommandona
  }
}
```

När du skickar kommandon från datorn till Arduino-enheten, skriv kommandot och tryck sedan på "Enter" eller "Return" - knappen för att skicka en radbrytning (`\n`). Se nedan för ett exempel på hur seriell kommunikation kan se ut:

```
5
3.14
```

Det första kommandot `5` läses in som ett heltal och sparas i variabeln `num`, medan det andra kommandot `3.14` läses in som ett decimaltal och sparas i variabeln `val`.

## Djupdykning:

Innan den digitala åldern, användes kommandoradsargument huvudsakligen för att köra program eller starta operativsystem via en kommandotolk eller terminal. Idag används det främst för att konfigurera och anpassa programvaror till specifika behov, och är en viktig del av många programmeringsspråk och plattformar, inklusive Arduino.

Alternativ till att använda `parseCommand()`-funktionen inkluderar att använda `Serial.parseInt()` och `Serial.parseFloat()`. Det är också möjligt att skicka flera kommandon i en sekvens och läsa in dessa genom att använda en loop eller en buffert. Det beror helt på hur du vill utforma din kod och vad som passar bäst för ditt projekt.

När du läser kommandoradsargument finns det några saker att tänka på. Först och främst måste du säkerställa att seriell kommunikation är aktiverad i din setup-funktion. Du bör också kontrollera att kommandona som skickas från datorn är korrekt formaterade och att ditt program kan hantera eventuella felaktiga eller ofullständiga kommandon.

## Se även:

- [Serial communication with Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino serial read functions](https://forum.arduino.cc/index.php?topic=229148.0)
- [Command-line interface](https://en.wikipedia.org/wiki/Command-line_interface)