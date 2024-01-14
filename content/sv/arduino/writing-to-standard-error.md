---
title:                "Arduino: Skriva till standardfel"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför
Att lära sig att skriva till standard error är en viktig del av att bli en skicklig Arduino-programmerare. Genom att kunna skriva till standard error kan du enkelt hitta och åtgärda fel i ditt program, vilket sparar tid och frustration.

## Hur man gör
För att skriva till standard error i Arduino behöver vi först definiera en instans av Serial-klassen för att kunna kommunicera med datorn via USB-porten. Sedan kan vi använda funktionen `Serial.println()` för att skriva ut vår önskade text till standard error.

```Arduino
Serial.begin(9600); // Instans av Serial-klassen
Serial.println("Det här är ett felmeddelande!"); // Skriver ut text till standard error
```

När koden körs kommer du att se felmeddelandet i fönstret för serieöverföring i Arduino-miljön.

## Deep Dive
Att skriva till standard error är ett enkelt sätt att felsöka ditt program. Genom att skriva ut värden på variabler eller meddelanden under körningen kan du enkelt se vad som händer och var eventuella problem uppstår. Det är också en bra idé att använda tydliga och beskrivande felmeddelanden för att underlätta felsökningen.

Ytterligare en fördel med att skriva till standard error är att det inte påverkar det faktiska arbetet som görs av ditt program. Om du till exempel använder `Serial.println()` för att kontrollera en variabel kan du enkelt ta bort koden eller kommentera ut den när du är klar med felsökningen.

## Se även
- [Arduino Reference - Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Serial.println() i Arduino](https://startingelectronics.org/software/arduino/learn-to-program-course/03-serial-print-functions/)
- [Skriva till standard error i Arduino](https://www.gavinbarron.com/skapa-error-reports-i-arduino/)