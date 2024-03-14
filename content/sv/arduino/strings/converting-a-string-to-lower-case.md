---
date: 2024-01-20 17:37:39.442593-07:00
description: "Att konvertera en str\xE4ng till gemener betyder att alla stora bokst\xE4\
  ver i texten omvandlas till sm\xE5 bokst\xE4ver. Programm\xF6rer g\xF6r detta f\xF6\
  r att standardisera\u2026"
lastmod: '2024-03-13T22:44:38.155045-06:00'
model: gpt-4-1106-preview
summary: "Att konvertera en str\xE4ng till gemener betyder att alla stora bokst\xE4\
  ver i texten omvandlas till sm\xE5 bokst\xE4ver. Programm\xF6rer g\xF6r detta f\xF6\
  r att standardisera\u2026"
title: "Konvertera en str\xE4ng till gemener"
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
