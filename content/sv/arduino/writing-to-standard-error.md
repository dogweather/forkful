---
title:                "Skriva till standardfel"
date:                  2024-01-19
simple_title:         "Skriva till standardfel"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva till standardfel (stderr) är att rikta felmeddelanden och diagnostik till en särskild utdatakanal, skild från huvuddataflödet (stdout). Programmerare använder detta för att separera felloggar från normal output, vilket gör felsökning och loggning mer överskådlig.

## Hur gör man:

Arduino plattformen har inte en dedikerad stderr kanal som mer komplexa operativsystem, men du kan simulera detta med Serial. Använd `Serial.print()` för normal output och definiera en funktion för felmeddelanden.

```arduino
void setup() {
  Serial.begin(9600); // Starta Serial-anslutningen
}

void loop() {
  Serial.println("Allt fungerar!"); // Normal output
  logError("Något gick fel!");    // Felmeddelande
  delay(2000); // Vänta 2 sekunder
}

void logError(String message) {
  Serial.println("ERROR: " + message); // Simulerad stderr output
}
```

Exempel på output:
```
Allt fungerar!
ERROR: Något gick fel!
```

## Djupdykning:

I början av datortiden skickades stderr och stdout till samma plats, ofta en teletypeskrivare. Man skilde dem åt för att enkelt kunna omdirigera loggar och felmeddelanden till olika destinationer, något som är standard i Unix-baserade system idag. På Arduino blir all output hanterad av Serial-klassen och går oftast till konsolen i IDE:n eller till en ansluten dator. För mer robusta system bör man implementa andra metoder för att hantera felloggar, som att skriva till en extern fil eller skicka datan över nätverk.

## Se även:

- Arduino Serial Library: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- ”Effective use of stderr” av Garrick Aden-Buie: https://garrickadenbuie.com/blog/effictive-use-of-stderr
- Wiki om standard streams: https://en.wikipedia.org/wiki/Standard_streams
