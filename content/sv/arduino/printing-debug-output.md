---
title:                "Arduino: Utskrift av felsökningsutdata"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva debug output är ett viktigt verktyg för att felsöka och förbättra din Arduino kod. Genom att skriva ut variabler, värden och meddelanden kan du bättre förstå vad som händer i din kod och identifiera eventuella problem. Det är också ett bra sätt att följa med i koden och se hur olika värden ändras under körningen.

## Så här gör du

För att skriva debug output i Arduino kan du använda funktionen "Serial.print()". Den tar som argument den variabel eller det värde du vill skriva ut. Du kan också använda "Serial.println()" för att lägga till en radbrytning efter varje utskrift.

```Arduino
int sensorValue = 0; // Skapa en variabel för att lagra sensordata
sensorValue = analogRead(A0); // Läs av analoga pinnen A0
Serial.print("Sensordata: "); // Skriv ut en text följt av värdet
Serial.println(sensorValue); // Skriv ut sensordata och en radbrytning
```

Det finns också möjlighet att använda "Serial.write()" för att skriva ut binär data, eller "Serial.printf()" för att formatera utskriften på ett mer specifikt sätt.

## Djupdykning

När du skriver debug output är det viktigt att tänka på hur det påverkar prestandan i din kod. Om du har för många utskrifter kan det leda till att din kod blir långsammare och eventuellt till och med orsaka krascher. Tänk därför på att ta bort eller kommentera ut alla utskrifter när du är klar med din kod.

En annan viktig aspekt är vilken hastighet du kommunicerar med din dator via seriel kommunikation. Standardhastigheten på Arduino är 9600 baud, men du kan ändra den med funktionen "Serial.begin()". Om du vill se debug output i realtid kan du också använda ett seriellt monitor-program på din dator, som till exempel Arduino IDE:s monitor, CoolTerm eller PuTTY.

## Se även

- [Arduino Serial Library Reference](https://www.arduino.cc/en/Reference/Serial)
- [Serial Communication with Arduino](https://www.arduino.cc/en/Tutorial/Serial)
- [Debugging Arduino Code with Serial Printing](https://create.arduino.cc/projecthub/joshi/debugging-arduino-code-with-serial-print-1d649e)