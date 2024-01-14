---
title:                "C++: Lage en midlertidig fil"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å opprette midlertidige filer kan være en nyttig teknikk i programmering for å behandle data midlertidig eller lagre midlertidige resultater uten å måtte lagre permanent på harddisken. Dette kan også være en effektiv måte å håndtere data som ikke trengs etter kjøring av programmet.

## Hvordan

For å opprette en midlertidig fil i C ++ kan vi bruke funksjonen `tmpfile`, som oppretter en tom midlertidig fil og returnerer en strømpeker til filen. Når programmet avsluttes, vil filen automatisk bli slettet fra systemet. Her er et eksempel på implementasjon:

```C++
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    fp = tmpfile(); // Opprette en midlertidig fil
    fprintf(fp, "Dette er en midlertidig fil."); // Skrive innhold i filen
    rewind(fp); // Flytte pekeren til starten av filen
    char c = fgetc(fp); // Lese enkelttegn fra filen
    printf("%c\n", c); // Skriver ut "D" som er det første tegnet i setningen
    fclose(fp); // Hvis filen ikke lenger er behov, må den lukkes og fjernes fra systemet
}
```

Utskrift: `D`

## Deep Dive

Funksjonen `tmpfile` oppretter en midlertidig fil i det midlertidige katalogen som er tilknyttet systemet. Denne katalogen kan være forskjellig fra system til system, men kan vanligvis finnes ved å bruke kommandoen `tmpdir` på Linux og `tempdir` på Windows. Det er også viktig å merke seg at midlertidige filer vil bli slettet automatisk ved systemstart, noe som betyr at de ikke er det beste alternativet for å lagre data som trenger å være tilgjengelig over flere kjøringer av programmet.

## Se også

- [C++ Reference - tmpfile](https://www.cplusplus.com/reference/cstdio/tmpfile/)
- [C++ Programming Language - Midlertidige filer](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)