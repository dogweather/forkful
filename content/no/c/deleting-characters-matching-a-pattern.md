---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sletting av tegn som samsvarer med et mønster, også kjent som mønstertilpasning, er en teknikk som brukes til å manipulere tekst. Programmerere bruker det for å effektivisere og forenkle arbeid med strenger.

## Slik Gjør Du Det:
Hvis du ønsker å slette tegn fra en streng som samsvarer med et gitt mønster i C, kan du bruke følgende kode:
```C
#include <string.h>
#include <stdio.h>

void DeleteMatchingChars(char *str, char *to_remove) {
  int src_index = 0, dest_index = 0;
  while (str[src_index]) {
    if (!strchr(to_remove, str[src_index])) {
      str[dest_index] = str[src_index];
      dest_index++;
    }
    src_index++;
  }
  str[dest_index] = '\0';
}

int main() {
  char str[] = "Hei verden!";
  DeleteMatchingChars(str, "e");
  printf("%s\n", str);
  return 0;
}
```
I dette eksemplet vil alle 'e'-tegn i strengen "Hei verden!" bli fjernet, og resultatet vil være: "Hi vrden!".

## Dypdykk
Metoden vi brukte over, ble utviklet i en tid da programmeringsspråk var mer begrenset. I dag finnes det mange alternativer for å oppnå det samme, som vanlige uttrykk (regular expressions) i språk som Python.

Imidlertid, gir C oss muligheten til å manipulere strenger på lavt nivå. I eksemplet ovenfor, gikk vi gjennom hver karakter og kopierte den tilbake til originalstrengen så lenge den ikke matchet mønsteret - en effektiv og direkte tilnærming.

Husk at hver gang du manipulerer en streng på denne måten, endrer du faktisk den opprinnelige dataen. Hvis du trenger originalstrengen, bør du ta vare på en kopi.

## Se Også
For mer informasjon, sjekk ut følgende kilder:

- "Mastering Algorithms with C" av Kyle Loudon: [Link](https://www.amazon.com/Mastering-Algorithms-C-Kyle-Loudon/dp/1565924533)
- C Programming på TutorialsPoint: [Link](https://www.tutorialspoint.com/cprogramming/index.htm)
- Stack Overflow for C programmeringsspørsmål: [Link](https://stackoverflow.com/questions/tagged/c)