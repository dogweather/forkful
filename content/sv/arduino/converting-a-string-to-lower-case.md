---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Så här konverterar du en sträng till små bokstäver i Arduino

### Vad & Varför?

En strängkonvertering till små bokstäver innebär att ändra alla versala tecken i en sträng till gemener. Programmerare gör detta för att standardisera dataingång och undvika case-sensitiva fel.

### Hur man gör:

I Arduino använder vi ofta "for" loopar för att göra detta. Låt oss ta en titt på följande exempel:

```Arduino
String str = "Hej Arduino!";
for (int i = 0; i < str.length(); i++) {
  str[i] = toLowerCase(str[i]);
}
Serial.println(str); // output: "hej arduino!"
```
Exemplet visar hur varje tecken i strängen omvandlas till en liten bokstav.

### Djupdykning

Strängkonvertering till små bokstäver är inte något nytt inom programmeringsvärlden. Konceptet har funnits så länge som programmeringsspråk har hanterat strängdata.

Arduino, till exempel, har inget inbyggt bibliotek för denna typ av uppgift, så vi måste implementera den själva, som vi gjorde i exemplet ovan. Synd, men detta är något Arduino kunde ha förbättrat.

Ett alternativ skulle vara att använda `toLowerCase()` funktionen för att konvertera hela strängen till små bokstäver. Men, denna metod är mindre effektiv än att använda en loop, eftersom den skapar en kopia av strängen istället för att ändra den på plats.

### Se även:

För mer fördjupad diskussion och exempel, kolla in följande källor:

- Arduino's String Object: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Stack Overflow Diskussion om ämnet: https://stackoverflow.com/questions/1779199/how-to-convert-string-to-lower-case-in-c