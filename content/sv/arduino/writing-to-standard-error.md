---
title:                "Att skriva till standardfel"
html_title:           "Arduino: Att skriva till standardfel"
simple_title:         "Att skriva till standardfel"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är ett användbart sätt att felsöka ditt Arduino-program. Det gör det möjligt att hitta och åtgärda eventuella fel och meddelanden som kan uppstå under körningen.

## Hur man gör

För att skicka meddelanden till standard error måste du använda funktionen `Serial.print()` och skicka med parametern `ARDUINO_ERROR`. Här är ett exempel på hur du kan skriva ut ett felmeddelande när en variabel inte är större än ett visst värde:

```Arduino
if (var < 10) {
  Serial.print("Error: Variabeln är inte större än 10. Värdet är: ");
  Serial.print(var, ARDUINO_ERROR);
  Serial.println();
}
```

Detta kommer att skriva ut följande till din Arduino IDE: 

```
Error: Variabeln är inte större än 10. Värdet är: 5
```

Genom att använda `ARDUINO_ERROR` som andra parameter till `Serial.print()` kommer du att se en röd varningstext bredvid ditt meddelande, vilket hjälper dig att hitta det snabbt.

## Djupdykning

När du skriver till standard error i Arduino, så skickas dina meddelanden till din dator via USB-porten. Detta gör att du kan se dem direkt i Arduino IDE. Du kan också använda verktyg som "Serial Monitor" för att visa dina meddelanden i realtid.

Det finns också möjlighet att skicka meddelanden till standard error på en annan enhet. Du kan använda en extern enhet som är ansluten via Wi-Fi eller Bluetooth för att se dina meddelanden.

## Se även 

- [Dokumentation om Serial.print()](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Arduino-tutorial för Serial Monitor](https://www.arduino.cc/en/Tutorial/SerialMonitor)
- [Guide för att använda externa enheter med Arduino](https://www.arduino.cc/en/Tutorial/TroubleshootingSerialCommunication)