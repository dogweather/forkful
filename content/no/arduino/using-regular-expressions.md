---
title:    "Arduino: Å bruke regulære uttrykk"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du driver med Arduino programmering, har du sannsynligvis vært borti en situasjon der du trenger å filtrere eller sortere data som kommer inn fra sensorer eller annet utstyr. Dette kan være en tidkrevende og kjedelig jobb hvis du må gjøre det manuelt. Den gode nyheten er at det finnes en måte å automatisere denne prosessen ved hjelp av såkalte regulære uttrykk, også kalt regex.

Regulære uttrykk er en måte å søke og manipulere tekststrenger på basert på et sett med regler og mønstre. Dette kan være svært nyttig for å finne og behandle spesifikke deler av en tekststreng, for eksempel tall, bokstaver eller spesielle tegn.

## Hva du trenger

For å kunne bruke regulære uttrykk i Arduino programmering, trenger du en kunnskapsrik tekstredigerer som støtter regex, for eksempel Visual Studio Code eller Sublime Text. Du vil også trenge å installere regex-biblioteket for Arduino, som er tilgjengelig på nettet.

## Hvordan bruke regular expressions

For å bruke regulære uttrykk i Arduino programmering, trenger du først å inkludere regex-biblioteket i koden din ved å legge til følgende linje øverst i koden:

```Arduino
#include <regex.h>
```

Deretter kan du definere et regulært uttrykk ved hjelp av følgende syntaks:

```Arduino
regex_t regex;
```

Du kan deretter bruke regex-funksjonene til å søke og manipulere tekststrenger basert på det definerte uttrykket. For eksempel kan du bruke funksjonen "regcomp" til å kompilere regulære uttrykk, og "regexec" til å utføre søk og manipulasjon på tekststrenger.

```Arduino
regex_t regex;

// Kompilerer uttrykket
int status = regcomp(&regex, "^H\\w+", 0);

// Utfører søk i en tekststreng
status = regexec(&regex, "Hei alle sammen!", 0, NULL, 0);
```

Output fra eksempelet over vil bli "Hei" siden uttrykket leter etter et ord som starter med en H og følges av en eller flere bokstaver.

## Grunnleggende regex mønstre

I tillegg til syntaksen som er nevnt over, er det også flere mønstre og regler du kan bruke i regulære uttrykk for å gjøre søkene dine mer spesifikke og nøyaktige. Her er noen av de vanligste mønstrene som kan være nyttige i Arduino programmering:

- \d - Søker etter tall (0-9)
- \w - Søker etter bokstaver (a-z, A-Z)
- \s - Søker etter mellomrom
- ^ - Starter et søk fra begynnelsen av en tekststreng
- $ - Slutter søket ved slutten av en tekststreng

Det finnes også mange flere mønstre og regler som kan være nyttige i ulike situasjoner, så det kan være lurt å gjøre litt research og prøve deg frem for å bli mer kjent med regex.

## Se også

- [Regex tutorial for Arduino](https://howtomechatronics.com/tutorials/arduino/regex-tutorial-regular-expressions/)
- [Official Arduino regex library](https://arduino.github.io/arduino-cli/0.25/libraries/regex/docs/regex.html)
- [Regular expression basics](https://www.regular-expressions.info/tutorial.html)