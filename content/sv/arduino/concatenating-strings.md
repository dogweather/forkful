---
title:                "Sammanfogning av strängar"
html_title:           "Arduino: Sammanfogning av strängar"
simple_title:         "Sammanfogning av strängar"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konkatenera strängar är när man kombinerar flera strängar till en enda sträng. Det kan vara användbart för att skapa dynamiska textmeddelanden, för att visa information till användaren eller för att bygga strängar för att skicka över kommunikationskanaler. Programmerare använder konkatenering för att effektivt hantera och manipulera text i sina program.

## Hur man gör:

```
// Skapa två strängar att konkatenera
String förnamn = "Anna";
String efternamn = "Andersson";

// Konkatenera strängarna med "+" -tecknet
String fulltNamn = förnamn + " " + efternamn;

// Skriv ut det konkatenerade namnet
Serial.println(fulltNamn);

// Resultat: Anna Andersson
```

## Djupdykning:

Historiskt sett har konkatenering använts för att spara dyrbar datalagring på datorer med begränsade resurser. Istället för att lagra flera separata strängar, kunde man konkatenera dem för att spara utrymme. Det finns också alternativ till konkatenering, som strängformatering och användning av teckenmatriser.

I Arduino kan strängar konkateneras med hjälp av + -operatören, men det är också värt att notera att varje gång du konkatenerar, skapas en ny sträng som tar upp mer minne. Så det är viktigt att tänka på minnesanvändningen i ditt program när du konkatenerar strängar.

## Se även:

- Tutorial: [String Concatenation](https://www.arduino.cc/en/Tutorial/StringConcatenation)
- Documentation: [String Operators](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)