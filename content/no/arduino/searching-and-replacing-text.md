---
title:    "Arduino: Søk og erstatt tekst"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

### Hvorfor

Å søke og erstatte tekst er en viktig del av programmering. Det gjør at du kan endre store deler av teksten i programmet ditt på en enkel og effektiv måte. Dette er spesielt nyttig når man jobber med store kodebaserte prosjekter.

### Hvordan

For å søke og erstatte tekst i Arduino, kan du bruke funksjonen "replace()" og "replaceIf()" fra "String" biblioteket. La oss se på et eksempel:

```Arduino
#include <String.h>

String tekst = "Hei, Arduino";
tekst.replace("Hei", "Hallo");

Serial.println(tekst);
```
Outputen vil være "Hallo, Arduino", der "Hei" er blitt byttet ut med "Hallo".

En annen måte å søke og erstatte tekst er ved hjelp av regular expressions. Dette er spesielle mønstre som gjør det mulig å søke etter bestemte ord eller uttrykk og erstatte med ønsket tekst. La oss se på et eksempel:

```Arduino
#include <regex>

String tekst = "Dette er en tekst som inneholder tall: 123456";

// Bytt ut alle tall med "X"
tekst = regex_replace(tekst, regex("[0-9]+"), "X");

Serial.println(tekst);
```
Outputen vil være "Dette er en tekst som inneholder tall: XXXXXX", der tallene er erstattet med "X".

### Dykk dypere

For å bli mer avansert med søk og erstatning av tekst, kan du også bruke funksjoner som "replaceAll()", "replaceFirst()" og "replaceLast()". I tillegg kan du kombinere disse funksjonene med "if()"-setninger for å søke etter spesifikke vilkår.

### Se også

- Dokumentasjon for String biblioteket: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Eksempler på regular expressions: https://www.regular-expressions.info/examples.html
- Tutorial om å søke og erstatte tekst i Arduino: https://www.arduino.cc/en/Tutorial/StringReplace?action=pages&view=old