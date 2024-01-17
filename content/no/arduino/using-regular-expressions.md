---
title:                "Å bruke regulære uttrykk"
html_title:           "Arduino: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

Å bruke regulære uttrykk er en metode for å søke og manipulere tekster ved hjelp av spesielle mønstre. Dette gjøres ofte av programmerere for å effektivt behandle store mengder tekst og utføre komplekse søk. Det kan også bidra til å redusere koden og gjøre den mer fleksibel.

Hvordan:

Arduino har et innebygd bibliotek, kalt "Regexp", som lar deg bruke regulære uttrykk i koden din. For å bruke dette, må du først inkludere biblioteket i koden din ved å skrive ```#include <Regexp.h>```. Deretter kan du opprette et nytt Regexp-objekt og definere mønsteret du vil søke etter. Her er et eksempel på hvordan du kan finne alle tall i en tekst og skrive dem ut:

```
Regexp pattern("(\d+)");
String tekst = "Det er 123 epler i kurven";
if (pattern.find(tekst)) {
  Serial.println(pattern.group(0)); // Skriver ut hele teksten som matcher mønsteret
  Serial.println(pattern.group(1)); // Skriver ut kun tallene som matcher mønsteret
}
```

Dette eksempelet vil skrive ut "123" to ganger.

Du kan også bruke regulære uttrykk til å endre teksten din. Her er et eksempel som erstatter alle forbudte ord med "*censor*":

```
Regexp pattern("forbudt ord");
String tekst = "Tekst med ett eller flere forbudte ord.";
tekst = pattern.replace(tekst, "*censor*");
Serial.println(tekst); // Skriver ut: Tekst med ett eller flere *censor*.
```

Dypdykk:

Konseptet med regulære uttrykk ble oppfunnet av matematikeren Stephen Kleene i 1951, og har blitt brukt i programmering siden 1960-tallet. Det finnes også andre måter å søke og manipulere tekster på, som for eksempel string funksjoner og strtok() funksjonen i Arduino. Men regulære uttrykk kan være mer fleksible og kraftige når det kommer til komplekse søk og manipulasjoner.

Se også:

Her er noen ressurser for å lære mer om regulære uttrykk:
- [Regexp bibliotekets dokumentasjon](https://www.arduino.cc/reference/en/libraries/regexp/)
- [RegExr - en interaktiv regulære uttrykk tester](https://regexr.com/)
- [Google Sheets RegEx funksjoner for å behandle tekst](https://www.benlcollins.com/formula-examples/google-sheets-regex-functions/)