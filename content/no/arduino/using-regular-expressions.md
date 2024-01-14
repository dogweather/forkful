---
title:                "Arduino: Å bruke regulære uttrykk"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Hvorfor

Å bruke regulære uttrykk i Arduino-programmering kan være en nyttig måte å håndtere og manipulere tekst. Det kan forenkle komplekse oppgaver som å søke etter spesifikke mønstre i en streng eller å filtrere ut uønsket data. Med regulære uttrykk kan du spare tid og få et mer effektivt program.

# Slik gjør du det

For å bruke regulære uttrykk i Arduino, må du først inkludere biblioteket "Regex" ved å bruke:

```Arduino
#include <Regex.h>
``` 

Deretter kan du definere et regulært uttrykk ved hjelp av funksjonen `Regex`:

```Arduino
Regex myRegex("pattern");
```

I dette eksempelet vil "pattern" være mønsteret du ønsker å finne i en streng.

Etter at du har definert uttrykket ditt, kan du bruke en av funksjonene som følger med "Regex" biblioteket for å søke etter eller manipulere tekst. For eksempel, hvis du vil søke etter et mønster i en tekststreng, kan du bruke `match()` funksjonen:

```Arduino
String input = "Det er 5 katter og 3 hundre i denne teksten.";
Boolean result = myRegex.match(input);
```

I dette tilfellet vil `result` være sann hvis mønsteret ble funnet i strengen `input`, eller usann hvis det ikke ble funnet.

# Dypdykk

Arduino har begrenset støtte for regulære uttrykk sammenlignet med andre programmeringsspråk, og kan ikke håndtere alle funksjoner og metoder som finnes i regex. Når du bruker regulære uttrykk i Arduino, må du huske på disse begrensningene og tilpasse uttrykkene dine deretter.

En annen ting å merke seg er at regex-biblioteket bruker mye Arduino-minne, så det kan være lurt å bruke det med forsiktighet og unngå det hvis du allerede har et begrenset minne.

# Se også

- [Offisiell Arduino Regex biblioteksdokumentasjon](https://github.com/arduino-libraries/Regex)
- [En guide til å bruke regulære uttrykk i Arduino](https://www.makeuseof.com/tag/using-regular-expressions-arduino/)
- [Eksempler på bruken av regulære uttrykk i Arduino-prosjekter](https://maker.pro/arduino/projects/how-to-use-regular-expressions-in-your-arduino-sketches)