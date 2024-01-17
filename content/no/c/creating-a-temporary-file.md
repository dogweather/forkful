---
title:                "Oppretting av en midlertidig fil"
html_title:           "C: Oppretting av en midlertidig fil"
simple_title:         "Oppretting av en midlertidig fil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Hva & hvorfor?
En midlertidig fil er en fil som opprettes og brukes midlertidig av et program under kjøring. Dette kan være nyttig for å lagre midlertidig data eller for å sikre at to eller flere programmer ikke konflikterer med hverandre ved å bruke samme filnavn. Programmere oppretter midlertidige filer for å gjøre programmene sine mer effektive og pålitelige.

# Hvordan:
```c
#include <stdio.h>
int main() {
  FILE *tfp = tmpfile(); // opprett en ny midlertidig fil
  if (tfp != NULL) {
    fputs("Dette er en midlertidig fil.", tfp); // skriv tekst til filen
    rewind(tfp); // gå tilbake til starten av filen
    char str[50];
    fgets(str, 50, tfp); // les filen og lagre teksten i str arrayen
    printf("Teksten i den midlertidige filen er: %s\n", str); // skriv ut teksten 
    fclose(tfp); // lukk filen
  } else {
    printf("Kunne ikke opprette en midlertidig fil.");
  }
  return 0;
}
```
Eksempelutgang:
```
Teksten i den midlertidige filen er: Dette er en midlertidig fil.
```

# Dykk dypere:
I tidligere tider var det vanlig for programmer å kommunisere ved hjelp av filer, og midlertidige filer ble brukt som en måte for programmer å sende data til hverandre. I dag brukes det ofte andre metoder som filpiping eller socket-kommunikasjon. Alternativer til å bruke midlertidige filer kan være å bruke minnebuffer eller å lagre data direkte i en database. Når du oppretter en midlertidig fil, blir den vanligvis lagret i operativsystemets TEMP-diskplassering. Implementeringen av midlertidige filer kan variere avhengig av operativsystem og programmeringsspråk, men konseptet er det samme.

# Se også:
- [tmpfile() function - GeeksforGeeks](https://www.geeksforgeeks.org/c-tmpfile-function/) 
- [Temporary File - Wikipedia](https://en.wikipedia.org/wiki/Temporary_file)