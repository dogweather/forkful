---
title:                "Arduino: Läsning av kommandoradsargument"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa kommandoradsargument kan vara en användbar funktion för både nybörjare och erfarna användare av Arduino-programmering. Genom att läsa argumenten som skickas med vid körning av programmet kan du anpassa ditt program för olika användningsscenarier eller göra det mer interaktivt.

## Hur man gör

Att läsa kommandoradsargument i Arduino är ganska enkelt. Du kan använda funktionen `Serial.readStringUntil()` för att läsa in argumenten och sedan omvandla dem till lämpliga datatyper.

```Arduino
void setup() {
  Serial.begin(9600);  // Öppnar seriell kommunikation
}

void loop() {
  String argument = Serial.readStringUntil('\n'); // Läser in argumentet fram till radbrytet
  int nummer = argument.toInt(); // Omvandlar argumentet till heltal
  Serial.println(nummer); // Skriver ut det inlästa argumentet
}
```

Om du till exempel skickar in "123" som argument vid körning av programmet, kommer programmet att skriva ut "123" i serial monitor. Du kan anpassa koden för att göra olika saker beroende på vilket argument som lästs in.

## Djupdykning

Det finns flera olika sätt att läsa kommandoradsargument på i Arduino, beroende på dina behov. Du kan till exempel använda `Serial.parseInt()` för att läsa in heltal eller `Serial.parseFloat()` för att läsa in flyttal. Det finns också möjlighet att läsa in flera argument samtidigt genom att använda en "delimiter" som skiljer argumenten åt. Detta kan vara praktiskt om du behöver läsa in flera värden på en gång.

## Se även

- [Dokumentation om Serial.readStringUntil()](https://www.arduino.cc/reference/en/language/functions/communication/serial/readstringuntil/)
- [Exempel på läsning av kommandoradsargument i Arduino](https://www.arduino.cc/en/Tutorial/CommandLineArguments/)
- [Diskussion och tips om läsning av kommandoradsargument i Arduino-forumet](https://forum.arduino.cc/index.php?topic=325524.0)