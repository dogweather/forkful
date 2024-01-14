---
title:    "Arduino: Bruk av regulære uttrykk"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang følt at koden din blir for rotete og vanskelig å lese? Eller at du trenger å søke gjennom store tekstfiler for spesifikke mønstre? Da er regular expressions et nyttig verktøy for å forenkle arbeidsflyten din som Arduino-programmerer.

## Hvordan

Å bruke regular expressions i Arduino er ganske enkelt. Først må du legge til biblioteket "Regex" i din Arduino-IDE. Deretter kan du bruke funksjonene fra dette biblioteket til å søke etter spesifikke mønstre i en tekststreng.

La oss si at du har en tekststreng som inneholder et tall og du bare er interessert i dette tallet. Du kan bruke regular expressions til å filtrere ut dette tallet ved å bruke følgende kode:

```
Arduino ...
#include <regex>
```
```
regex reg("([0-9]+)"); // lager et mønster for å matche tall
String streng = "Dette er en tekststreng med 123 et tall i den";
match_results<String::const_iterator> resultater; 
// lagrer resultatet av søket i en variabel
if (regex_search(streng, resultater, reg)) {
    for (int i = 0; i < resultater.size(); ++i) {
        Serial.println(resultater[i].str().c_str()); // skriver ut det matchende tallet
    }
}
```

Dette vil skrive ut "123" i Serial Monitor. Du kan også bruke regular expressions med variabler som temperatursensorer eller knapper, for å filtrere ut spesifikke verdier fra sensordata.

## Dypdykk

Regular expressions kan bli enda mer kraftfulle ved å bruke såkalte "metakarakterer". Dette er spesielle symboler som representerer flere tegn samtidig. For eksempel kan du bruke "." for å matche ett hvilket som helst tegn eller "*" for å matche hvilken som helst mengde av et visst tegn.

Du kan også bruke uttrykk som "[]", som representerer en gruppe av tegn. For eksempel [a-z] vil matche alle små bokstaver, mens [0-9] vil matche alle tall.

Det finnes også en rekke nyttige funksjoner i dette biblioteket, som regex_replace() for å erstatte tekst og regex_match() for å sjekke om en streng matcher et gitt mønster.

## Se også

- [Arduino Regex-bibliotek](https://www.arduino.cc/reference/en/libraries/regex/)
- [RegExr](https://regexr.com/), et nyttig verktøy for å teste og eksperimentere med regular expressions
- [Dokumentasjon for Regular Expressions](https://www.regular-expressions.info/), mer informasjon om hvordan du bruker regular expressions i ulike programmeringsspråk