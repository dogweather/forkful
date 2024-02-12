---
title:                "Konvertera en sträng till gemener"
date:                  2024-01-20T17:37:39.442593-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertera en sträng till gemener"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener betyder att alla stora bokstäver i texten omvandlas till små bokstäver. Programmörer gör detta för att standardisera data, till exempel när man jämför strängar och det är oviktigt med versaler.

## How to:
```Arduino
void setup() {
  Serial.begin(9600);
  String myString = "Hej, VÄRLDEN!";
  myString.toLowerCase();
  Serial.println(myString);
}

void loop() {
  // Vi kommer inte att använda loop i det här exemplet.
}
```
Output:
```
hej, världen!
```

## Deep Dive
Strängkonvertering finns i många programmeringsspråk och introducerades för att hantera versalkänsligheten i text. Alternativ till `.toLowerCase()` i Arduino inkluderar att manuellt genomgå varje tecken i strängen och använda funktionen `tolower()` som finns i C Standard Library för att omvandla det.

För att implementera omvandlingen innebär det att Arduino går igenom strängen tecken för tecken. ASCII-värdet för stora bokstäver justeras till deras motsvarande små bokstävers värden. Detta skapar en standardisering av text för bearbetning, därför att 'A' och 'a' inte ska tolkas som olika tecken i operationer som inte är beroende av versaler.

## See Also
- Arduino's official `String` reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- C Standard Library `tolower()` function: http://www.cplusplus.com/reference/cctype/tolower/
