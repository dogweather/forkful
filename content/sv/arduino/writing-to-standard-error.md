---
title:                "Arduino: Att skriva till standardfel"
simple_title:         "Att skriva till standardfel"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standardfel kan vara en användbar funktion i ett Arduino-program. När man skriver till standardfel så visar man ett felmeddelande i konsolfönstret, vilket kan hjälpa till att identifiera eventuella problem i koden.

## Hur man gör det

För att skriva till standardfel i Arduino så använder man funktionen `Serial.println()` och anger `Serial` som det första argumentet. Man kan också använda `Serial.print()` för att skriva ut texten utan radbrytning.

```Arduino
Serial.println("Detta är ett felmeddelande"); //skriver ut ett meddelande med radbrytning
Serial.print("Detta är en debuggningstext"); //skriver ut en debuggningstext utan radbrytning
```

När man sedan kör sitt program så kommer dessa meddelanden att visas i konsolfönstret.

## Djupdykning

Att skriva till standardfel kan vara speciellt användbart vid debugging av komplexa program. Genom att placera `Serial.println()` instruktioner vid olika delar av koden så kan man se vilka delar som utförs och i vilken ordning.

Man kan också använda `Serial.begin()` funktionen för att specificera en baud-rate för kommunikationen till konsolfönstret. Detta kan vara särskilt användbart om man arbetar med en annan hårdvaruplattform än den som används för att visa meddelandena.

## Se även

- [Arduino Serial.println() funktion](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Arduino Serial.print() funktion](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Arduino Serial.begin() funktion](https://www.arduino.cc/reference/en/language/functions/communication/serial/begin/)