---
title:                "Att göra om en sträng till versaler"
html_title:           "Arduino: Att göra om en sträng till versaler"
simple_title:         "Att göra om en sträng till versaler"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att *kapitalisera en sträng* betyder helt enkelt att göra alla bokstäver i en sträng till versaler (stora bokstäver). Detta är en vanlig uppgift för programmerare när de behöver formatera data eller användarinmatning på ett enhetligt sätt. Genom att göra om alla bokstäver till versaler, ser strängen snyggare ut och blir lättare att läsa.

## Så här:

Här är ett enkelt exempel på hur man kapitaliserar en sträng i Arduino:

```
// Deklarera en sträng som ska kapitaliseras
String minStrang = "hej, detta är en sträng";

// Skriv ut den ursprungliga strängen
Serial.println(minStrang);

// Använd metoden toUpperCase () för att göra alla bokstäver till versaler
String kapitalStrang = minStrang.toUpperCase();

// Skriv ut den kapitaliserade strängen
Serial.println(kapitalStrang);
```

**Output:**

```
hej, detta är en sträng
HEJ, DETTA ÄR EN STRÄNG
```

## Djupdykning:

Att kapitalisera en sträng är en vanlig uppgift inom programmering och finns tillgänglig i många programmeringsspråk, inklusive Arduino. Det finns dock också andra sätt att formatera strängar, till exempel genom att göra alla bokstäver till gemener (små bokstäver) eller genom att bara förändra den första bokstaven till versal och resten till gemener.

För att implementera kapitalisering i Arduino använder vi den inbyggda metoden toUpperCase (), som är speciellt utformad för strängar. Denna metod kan användas på en befintlig sträng eller på en sträng som skapas i koden.

## Se även:

För mer information om strängmanipulering i Arduino, kolla in [Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/) för dokumentation om toUpperCase () metoden. Du kan också utforska fler sätt att formatera och manipulera strängar i Arduino genom att titta på andra inbyggda metoder som toLowerCase () och substring (). Lycka till med att kapitalisera dina strängar!