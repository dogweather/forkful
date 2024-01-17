---
title:                "Användning av reguljära uttryck"
html_title:           "Arduino: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck är ett sätt för programmerare att söka och manipulera textsträngar. Det kan vara användbart när du behöver hitta ett specifikt mönster i en text eller vill göra stora ändringar på flera textsträngar med en enda kodrad.

## Så här:
För att använda reguljära uttryck i Arduino behöver du först inkludera "Regex" biblioteket. Sedan kan du använda funktionen "match()" för att söka efter ett mönster i en textsträng.

```
#include <Regex.h>

void setup() {
  // Skapa en textsträng att söka i
  String text = "Hej! Mitt namn är Bob.";
  
  // Skapa ett reguljärt uttryck
  Regex pattern = Regex("[a-z]+!");
  
  // Använd match() för att söka efter mönstret i textsträngen
  if (pattern.match(text)) {
    // Om mönstret hittas, skriv ut matchningen
    Serial.println(pattern.matched()); // Skriver ut "Hej!"
  }
}

void loop() {
  // Kör ingenting i loop
}
```

## Djupdykning:
Reguljära uttryck har funnits sedan 1950-talet och används idag i många programmeringsspråk. Alternativ till RegEx är strängfunktioner som "find()" och "replace()", men de är mindre flexibla och kräver mer kodrad för att åstadkomma samma sak.

## Se även:
Läs mer om hur man använder reguljära uttryck i Arduino på officiella dokumentationen: https://www.arduino.cc/reference/en/language/functions/strings/match/

En praktisk guide för hur man använder reguljära uttryck på engelska: https://www.regular-expressions.info/arduino.html