---
title:                "Bruk av regulære uttrykk."
html_title:           "Arduino: Bruk av regulære uttrykk."
simple_title:         "Bruk av regulære uttrykk."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor 
Bruk av regulære uttrykk, også kjent som regex, kan være en effektiv måte å søke, filtrere og manipulere tekstbaserte data på. Dette er spesielt nyttig når man jobber med store datasett eller kompleks tekstbehandling, slik som i programmering. Ved å lære å bruke regex, kan du gjøre arbeidet ditt mer effektivt og effektivt.

## Hvordan 
Først må du inkludere innebygd funksjonalitet for regulære uttrykk i koden din ved å skrive: ```Arduino	
#include <regex.h>```
Nå kan du begynne å bruke regex i koden din. For eksempel, hvis du ønsker å finne alle tall i en tekststreng og lagre dem i en variabel, kan du bruke følgende kode:
```
Arduino
char string[] = "I dag er det 25. september 2021";
regex_t regex;
int status = regcomp(&regex, "[0-9]+", 0);
regmatch_t matches[1];
status = regexec(&regex, string, 1, matches, 0);
char numbers[matches[0].rm_eo - matches[0].rm_so + 1];
strncpy(numbers, string + matches[0].rm_so, matches[0].rm_eo - matches[0].rm_so);
```
Nå vil variabelen numbers inneholde tallet "25". Her har vi brukt regex-uttrykket "[0-9]+" som betyr at vi ønsker å finne alle tall som består av en eller flere sifre i den gitte teksten. Dette er bare ett eksempel, og det finnes mange forskjellige regex-uttrykk som kan brukes avhengig av hvilken tekst du jobber med og hva du ønsker å oppnå.

## Dypdykk 
Regex kan være en utfordrende konsept å lære fordi det er mange ulike symboler og uttrykk å huske. Det kan også være vanskelig å teste regex i sanntid siden det ofte krever å følge en bestemt syntaks. Derfor kan det være lurt å bruke en online regex tester som [regex101](https://regex101.com/) når du skal utvikle og teste regex-uttrykk for å sikre at de fungerer som forventet. Det finnes også mange ressurser på nettet som kan hjelpe deg med å lære mer om regex og hvordan du kan bruke det på en effektiv måte i koden din.

## Se også 
- [regex101](https://regex101.com/)
- [Arduino reference for regex](https://www.arduino.cc/reference/en/language/variables/utilities/regex/)
- [The Absolute Minimum Every Software Developer Absolutely, Positively Must Know About Unicode and Character Sets (No Excuses!)](https://www.joelonsoftware.com/2003/10/08/the-absolute-minimum-every-software-developer-absolutely-positively-must-know-about-unicode-and-character-sets-no-excuses/)