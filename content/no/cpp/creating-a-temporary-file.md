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

## Hvorfor

Å lage midlertidige filer kan være nyttig når man trenger å lagre data midlertidig, for eksempel ved kjøring av et program eller når man leser eller skriver data til en fil.

## Hvordan lage midlertidige filer i C++

Måten å lage en midlertidig fil på i C++ er ved å bruke standardbiblioteket <fstream>. Vi må inkludere dette biblioteket i koden vår for å kunne lage og manipulere filer. Deretter bruker vi funksjonen "tmpfile()" for å opprette vår midlertidige fil, som vil bli lagret i operativsystemets midlertidige mappe.

```C++
#include <fstream>
using namespace std;

int main() {
  // Opprett en midlertidig fil
  FILE *fp = tmpfile();
  
  // Skriv tekst til filen
  fprintf(fp, "Dette er en midlertidig fil.");
  
  // Les fra filen
  fseek(fp, 0, SEEK_SET);
  char buffer[100];
  fgets(buffer, 100, fp);
  
  // Print ut teksten
  cout << buffer << endl;
  
  return 0;
}
```

Output:

```Dette er en midlertidig fil.```

I dette eksemplet oppretter vi en midlertidig fil ved hjelp av "tmpfile()", skriver teksten "Dette er en midlertidig fil." til filen og leser deretter teksten fra filen og skriver den ut på skjermen. For å sikre at filen blir slettet når programmet er ferdig, kan vi bruke funksjonen "fclose()" for å lukke filen.

## Dypdykk i oppretting av midlertidige filer

Når vi bruker funksjonen "tmpfile()" for å opprette en midlertidig fil, vil filen bli lagret i operativsystemets midlertidige mappe. Dette er vanligvis i "/tmp" for Linux og "/var/tmp" for macOS. Operativsystemet vil generere et unikt filnavn for den midlertidige filen og returnere en peker til filen som vi kan bruke i koden vår.

Det er også verdt å merke seg at når programmet vårt avsluttes, vil filen automatisk bli slettet fra den midlertidige mappen. Dette er nyttig for å spare plass på harddisken og unngå rot med unødvendige midlertidige filer.

## Se også

- https://www.cplusplus.com/reference/cstdio/tmpfile/
- https://en.cppreference.com/w/c/io/tmpfile