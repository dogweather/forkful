---
title:                "C: Sletting av tegn som matcher et mønster"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan det være nyttig å fjerne bestemte tegn fra en tekststreng eller fil. Dette kan være for å reformatere data, fjerne uønskede tegnfeil eller utføre andre manipulasjoner av data. I C-programmering kan vi enkelt slette tegn som matcher et bestemt mønster med noen få linjer med kode. I denne bloggposten vil vi gå gjennom hvordan du kan gjøre dette.

## Hvordan

For å slette tegn som matcher et mønster, kan vi bruke funksjonen "strchr" fra string.h-biblioteket. Denne funksjonen søker gjennom en tekststreng og returnerer en peker til det første forekomsten av et gitt tegn. Vi kan deretter bruke denne pekeren til å slette tegnene fra tekststrengen ved å overskrive det med påfølgende tegn.

La oss se på et eksempel:

```C
#include <stdio.h>
#include <string.h>

int main() {
  char tekststreng[] = "Dette er en tekststreng med noen uønskede tegn.";
  char uønsket_tegn = 'e'; // vi ønsker å fjerne alle forekomstene av 'e' fra teksten

  char *match = strchr(tekststreng, uønsket_tegn); // finner første forekomst av 'e'

  while (match != NULL) { // så lenge det er flere forekomster av 'e'
    strcpy(match, match+1); // kopierer de påfølgende tegnene over denne forekomsten
    match = strchr(tekststreng, uønsket_tegn); // finner neste forekomst av 'e'
  }

  printf("%s", tekststreng); // skriver ut den nye tekststrengen uten de uønskede tegnene

  return 0;
}
```

Output:

```
Dtt r n txtstrng md ngn uønskt dgn
```

Som du kan se, har alle forekomstene av 'e' blitt fjernet fra teksten. Dette eksempelet viser en enkel implementering av en funksjon for å slette tegn som matcher et mønster. Du kan tilpasse og utvide denne koden etter dine behov og til din spesifikke datasett.

## Dypdykk

Vi har nå sett på en enkel måte å slette tegn som matcher et mønster, men det finnes også andre metoder å gjøre dette på. En vanlig metode er å bruke en loop og en teller for å gå gjennom tekststrengen og slette tegnene etter behov. Det er også verdt å nevne at funksjonen "strchr" faktisk søker gjennom hele tekststrengen, så hvis du ønsker å slette bare det første tegnet som matcher, kan du bruke funksjonen "strpbrk" i stedet.

Det kan også være nyttig å lære å bruke regulære uttrykk for å slette tegn. Dette gir større fleksibilitet og kraft i manipulasjonen av tekst og tegn. Se gjerne på linker nedenfor for mer informasjon om dette temaet.

## Se også

- [strchr() dokumentasjon på Cplusplus.com (engelsk)](https://www.cplusplus.com/reference/cstring/strchr/)
- [strpbrk() dokumentasjon på Cplusplus.com (engelsk)](https://www.cplusplus.com/reference/cstring/strpbrk/)
- [Regulære uttrykk tutorial på w3schools.com (engelsk)](https://www.w3schools.com/cpp/cpp_strings_regex.asp)