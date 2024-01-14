---
title:    "Arduino: Skrivning till standardfel"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är ett viktigt steg i att debugga och förbättra din Arduino-kod. Genom att kunna hantera felmeddelanden som visas i din serieport kan du identifiera och åtgärda problem i din kod snabbare.

## Så här gör du

Att skriva till standard error i Arduino är enkelt. Du kan använda funktionen `print()` eller `println()` och lägga till bokstäverna `err` framför ditt meddelande.

```Arduino
Serial.print("err"); // Skriver bara "err" till standard error
Serial.println("err Felmeddelande"); // Skriver "err Felmeddelande" och hoppar till nästa rad på serieporten
```

En annan användbar funktion är `errno()` som returnerar ett nummer som motsvarar det senaste felmeddelandet. Det är användbart om du vill använda det inom en `if`-sats eller behöver visa det specifika felet i din kod.

``` Arduino
if(errno() == 1) {   // Kontrollerar om det senaste felet var nummer 1
    Serial.println("err Felmeddelande 1"); // Skriver ut en anpassad felmeddelande
}
```

## Deep Dive

När du skriver till standard error används den inbyggda klassen `Serial`. Om du vill skriva till en annan seriell port, till exempel en LCD-skärm, måste du först använda funktionen `begin()` för att starta serialkommunikationen på den specifika porten.

```Arduino
Serial1.begin(9600); // Initierar kommunikation på serielport 1 med en baudhastighet på 9600
Serial1.print("err Felmeddelande"); // Skriver till serielport 1 istället för standard error
```

Det finns också möjlighet att ändra bokstäverna `err` genom att redigera den inbyggda definitionen av `err` i filen "HardwareSerial.cpp".

Att använda `fprintf()` är ett annat alternativ för att skriva till standard error. Detta kan vara enklare om du behöver formatera ditt felmeddelande med variabler. Detta kräver dock lite mer kod och lägger till en extern beroende till din Arduino-kod.

```Arduino
// Kräver inkludering av <stdio.h> och <errno.h>
fprintf(stderr, "err Felmeddelande %d", errno()); // Skriver till standard error med format och variabler
```

## Se även

- [Arduino Dokumentation om seriell kommunikation](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino Reference om seriell kommunikation](https://www.arduino.cc/en/Reference/Serial)
- [Stack Overflow - How to write to standard error?](https://stackoverflow.com/questions/11192407/how-to-write-to-standard-error)