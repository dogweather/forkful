---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sammensetting av strenger, altså at vi driver med det vi kaller 'konkatenering', er en teknikk der vi fester eller kobler sammen to eller flere strenger til én. Dette er nødvendig for å strukturere og formatere output på en meningsfull måte i mange programmeringsscenarier.

## Hvordan gjøre det:

Her er en grunnleggende Arduino-kode for å sammenfeste to strenger:

```Arduino
String streng1 = "Hei, ";
String streng2 = "verden!";
String samletStreng = streng1 + streng2;  // "Hei, verden!"
```

Her er et annet eksempel:

```Arduino
String streng1 = "Temp: ";
float temperatur = 21.5;
String melding = streng1 + String(temperatur);  // "Temp: 21.5"
```

## Dypdykk

Selv om sammenføring av strenger virker enkelt, er det litt mer til dette enn det ser ut til. I den tidlige historien av programmeringsspråk, skjedde konkatenering ved å arbeide direkte med minnet. 

Alternativer til operatøren '+' for sammensetting er funksjonene 'concat()', 'sprintf()' etc. Men '+' operatøren gir en enklere og mer intuitiv måte å slå sammen strenger på. 

Arduino implementerer konkatenering ved bruk av '+' operatøren i String-klassen, noe som gjør det enkelt og likefremt å bruke, selv om det under overflaten ligger litt kompleksitet.

## Se også:

Besøk disse linkene for mer informasjon og relaterte emner:

1. For mer om strenger i Arduino: [Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/).
2. For å mestre '+' operatør for strenger: [Arduino String Addition Operator](https://www.arduino.cc/reference/en/language/variables/data-types/string/operators/addition/).
3. Mer om forskjellige funksjoner som concat og sprintf: [Arduino String Concat Function](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/).