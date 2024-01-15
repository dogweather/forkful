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

# Varför

Att läsa kommandoradsargument kan vara användbart i Arduino-programmering för att göra koden mer flexibel och anpassningsbar. Det kan också vara användbart för att skicka kommandon eller parametrar till din Arduino via en extern enhet eller program.

## Hur du gör det

För att läsa kommandoradsargument i Arduino, behöver du först inkludera biblioteket "Arduino.h". Sedan kan du använda funktionen "Serial.readString()" för att läsa de inkommande kommandona som en sträng.

```arduino
#include <Arduino.h>

void setup() {
  // Initiera seriell kommunikation
  Serial.begin(9600);
}

void loop() {

  // Läser inkommande kommandoradsargument som en sträng
  String input = Serial.readString();

  // Skriver ut inkommande kommando
  Serial.println(input);
}
```

Om du skickar "LED ON" till din Arduino via den seriella kommunikationen, kommer det att skrivas ut "LED ON" i seriell monitor.

## Djupdykning

Du kan också använda funktionen "Serial.parseInt()" för att läsa inkommande nummer eller använda olika argument för att läsa inkommande bokstäver som t.ex "Serial.read()". Du kan också lägga till felhantering för att hantera ogiltiga kommandon eller argument.

# Se även

- [Arduino Serial Programming](https://www.arduino.cc/en/Serial/Read)
- [Arduino Reference - Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino Forum - Reading Arguments](https://forum.arduino.cc/index.php?topic=335535.0)