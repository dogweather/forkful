---
title:                "Søking og utskifting av tekst"
html_title:           "Arduino: Søking og utskifting av tekst"
simple_title:         "Søking og utskifting av tekst"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

### Hva og hvorfor?
Å søke og erstatte tekst er en vanlig oppgave for programmerere. Dette innebærer å finne et bestemt uttrykk eller ord i en tekst og erstatte det med et annet. Dette kan være nyttig for å rette opp feil, endre i koden eller gjøre omfattende endringer i en fil.

## Hvordan:
For å søke og erstatte tekst på en effektiv måte i Arduino, kan du bruke funksjonen "replace" og "replaceAll" fra String-objektet. Se eksemplene nedenfor for å se hvordan dette fungerer.

```
// Eksempel 1:
String tekst = "Hei verden!";
tekst.replace("verden", "univers");
Serial.println(tekst);
// Output: Hei univers!

// Eksempel 2:
String telefonnummer = "888-888-8888";
String nyttNummer = telefonnummer.replaceAll("-", "");
Serial.println(nyttNummer);
// Output: 8888888888
```

## Dypdykk:
I eldre versjoner av Arduino var det vanlig å bruke funksjoner som "strreplace" eller "strtok" for å søke og erstatte tekst. Disse er fortsatt tilgjengelige, men det anbefales å bruke "replace" og "replaceAll" da de er mer effektive og enklere å bruke.

Det finnes også flere biblioteker tilgjengelig for å håndtere søking og erstatting av tekst, som for eksempel "ArduinoStrings", "StringMap" og "Festring".

Det er også verdt å merke seg at å bruke replace-funksjonene på store tekstfiler kan være tidskrevende og derfor ikke anbefalt. Det kan være bedre å bruke tekstbehandlingsverktøy som grep eller sed for å håndtere store filer.

## Se også:
- [Arduino referansedokumentasjon for String-objekt](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [ArduinoStrings bibliotek](https://github.com/JChristensen/ArduinoString)
- [StringMap bibliotek](https://github.com/millerlp/ArduinoStringMap)
- [Festring bibliotek](https://github.com/Winterland1989/Festring)