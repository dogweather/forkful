---
title:                "Sletting av tegn som matcher et mønster"
html_title:           "C: Sletting av tegn som matcher et mønster"
simple_title:         "Sletting av tegn som matcher et mønster"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Sletting av tegn som samsvarer med et mønster er en vanlig oppgave innen programmering. Dette er når man fjerner en bestemt type tegn fra en tekststreng basert på et definert mønster. Dette gjøres vanligvis for å strukturere og rense data, eller for å lage spesifikke output.

## Hvordan:
```
#include <stdio.h>

int main() {

  char string[] = "Dette er en tekststreng med tall 123 og symboler !@#";

  // Fjerne tall fra strengen
  for (int i = 0; string[i] != '\0'; i++) {
    if (string[i] >= '0' && string[i] <= '9') {
      // Bytt ut tallet med mellomrom
      string[i] = ' ';
    }
  }

  // Skriv ut den oppdaterte strengen
  printf("%s", string);
  return 0;
}
```

Output:
```
Dette er en tekststreng med tall    og symboler !@#
```

## Dypdykk:
Sletting av tegn basert på et mønster har eksistert siden de tidligste dagene av programmering når man jobbet med tekstbaserte systemer. Alternativer til å bruke et programmeringsspråk som C for å utføre denne oppgaven inkluderer bruk av kommandolinje-verktøy som "sed" og "awk". Implementasjonen av en slik funksjon kan variere basert på programmeringsspråk og algoritme som brukes, men det viktigste konseptet forblir det samme.

## Se også:
- [C programmeringsspråk](https://no.wikipedia.org/wiki/C_(programmeringsspr%C3%A5k))
- [SED - tekstmanipulering via kommandolinje](https://no.wikipedia.org/wiki/Sed)