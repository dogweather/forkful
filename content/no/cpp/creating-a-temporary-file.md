---
title:                "Oppretting av en midlertidig fil"
html_title:           "C++: Oppretting av en midlertidig fil"
simple_title:         "Oppretting av en midlertidig fil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å opprette en midlertidig fil i programmering betyr å opprette en midlertidig lagringsplass for data som kan brukes senere i koden. Dette is en vanlig praksis blant programmere for å organisere og håndtere data på en effektiv måte.

## Hvordan:

For å opprette en midlertidig fil i C++, kan du bruke funksjonen "tmpfile()" som er tilgjengelig i standardbiblioteket. Denne funksjonen vil automatisk opprette en midlertidig fil og returnere en filpeker som du kan bruke til å lese eller skrive til filen. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```C++
#include <stdio.h>
#include <stdlib.h>

int main() {
  FILE *tmp = tmpfile(); // Oppretter en midlertidig fil og lagrer filpekeren i variabelen "tmp"
  fprintf(tmp, "Dette er innholdet i den midlertidige filen"); // Skriver data til den midlertidige filen
  fclose(tmp); // Lukker filen når den ikke lenger er i bruk
  return 0;
}
```

Output:
-Ingen output vil vises, men filen vil bli opprettet i et midlertidig område på datamaskinen og data vil bli skrevet til den.

## Dykk dypere:

Å opprette midlertidige filer har vært en vanlig praksis blant programmerere siden begynnelsen av 1960-tallet. Det ble først introdusert i programmeringsspråket "BCPL" og har siden blitt adoptert i mange andre språk, inkludert C++. Det finnes også alternative måter å opprette midlertidige filer på, som for eksempel å bruke en fil med et tilfeldig navn som blir slettet når den ikke lenger er i bruk.

Når en midlertidig fil blir opprettet, vil systemet automatisk generere et unikt navn for filen som vil være forskjellig fra andre eksisterende filer. I tillegg vil systemet sørge for å slette filen når programmet termineres, slik at man unngår å få igjenliggende midlertidige filer på datamaskinen.

## Se også:

Hvis du vil lære mer om håndtering av filer i C++, kan du se vår artikkel "Hvordan lese og skrive til filer i C++". Eller hvis du vil vite mer om C++ funksjoner, kan du sjekke ut vår "C++ Funksjoner Guide" for en oversikt over ulike funksjoner og hvordan du bruker dem i koden din.