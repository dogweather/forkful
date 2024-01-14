---
title:                "Arduino: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Varför?

När vi programmerar kommer vi ibland över situationer där vi behöver läsa in argument från kommandoraden. Det kan vara ett sätt att interagera med vårt program på ett mer dynamiskt sätt. I denna bloggpost kommer vi att lära oss hur man kan läsa in kommandoradsargument i Arduino.

# Så här gör du

Först måste vi inkludera standardbiblioteket `CommandLine` som ger oss möjlighet att läsa in kommandoradsargument. Sedan behöver vi initiera en instans av `CommandLine` och definiera vilka argument vi vill läsa in. I exemplet nedan läser vi in två argument - ett heltal och en sträng.

```Arduino
#include <CommandLine.h>

CommandLine cmd; // Initiera instans av CommandLine

void setup() {
  // Definiera argument - ett heltal och en sträng
  cmd.addOption("intArg", 'i');
  cmd.addOption("strArg", 's');
}

void loop() {
  // Kontrollera om det finns kommandoradsargument tillgängliga
  if (cmd.readCommand()) {
    // Läs in argumenten och spara dem i variabler
    int num = cmd.getValue("intArg");
    String str = cmd.getValue("strArg");
    
    // Skriv ut argumenten till serien
    Serial.print("Heltal: ");
    Serial.println(num);
    Serial.print("Sträng: ");
    Serial.println(str);
  }
}
```

Om vi nu till exempel kör vårt program med kommandoradsargumentet `-i 10 -s hej`, kommer vi att få följande utskrift i seriell monitor:

```
Heltal: 10
Sträng: hej
```

# Deep Dive

Det finns även andra funktioner som finns tillgängliga i `CommandLine`-biblioteket. Till exempel så kan vi kontrollera om ett specifikt argument finns med i kommandoraden med hjälp av `hasOption()` funktionen och vi kan även återställa alla argument med `clear()` funktionen. För mer detaljerad information kan du läsa dokumentationen för `CommandLine`-biblioteket.

# Se även

- [Dokumentation för CommandLine-biblioteket](https://github.com/arduino-libraries/CommandLine)
- [Tutorial om användning av kommandoradsargument i Arduino](https://www.arduino.cc/en/Tutorial/CommandLineArguments)