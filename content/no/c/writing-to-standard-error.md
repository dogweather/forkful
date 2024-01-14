---
title:    "C: Skrive til standardfeil"
keywords: ["C"]
---

{{< edit_this_page >}}

## Hvorfor

Skriving til standardfeilsøkingskanaler, som standard error, er en viktig del av C-programmering. Det lar utviklere fange og feilsøke potensielle feil i programmet sitt.

## Hvordan

Skriving til standard error i C-programmering er enkelt og krever bare en linje med kode som sender en melding til denne kanalen.

```C
fprintf(stderr, "Feil oppstod: Dette er bare et eksempel.");
```

Dette eksempelet bruker fprintf-funksjonen til å sende meldingen "Feil oppstod: Dette er bare et eksempel" til standard error. Det er viktig å merke seg at vi bruker "stderr" som et argument for å sikre at meldingen faktisk blir sendt til standard error og ikke en annen kanal.

For å kunne bruke fprintf, må du også inkludere <stdio.h> biblioteket i koden din ved å skrive følgende linje på toppen:

```C
#include <stdio.h>
```

Når programmet kjøres, vil meldingen bli sendt til standard error, og hvis det oppstår en feil, vil denne meldingen bli inkludert i feilmeldingen som vises. Her er et eksempel på hvordan dette kan se ut når programmet kjøres:

```
Feil oppstod: Dette er bare et eksempel.
```

## Dypdykk

Å forstå hvordan man bruker standard error er viktig for å kunne feilsøke C-programmer. Ved å sende meldinger til denne kanalen, kan utviklere få informasjon om hvor og når feil oppstår i koden deres.

Det er også viktig å merke seg at andre kanaler som standard output (stdout) også kan brukes til å sende meldinger, men standard error er spesielt nyttig for å skille mellom vanlig programutgang og feilinformasjon.

I tillegg til å bruke fprintf-funksjonen, kan du også bruke perror-funksjonen for å få en mer detaljert feilmelding. Denne funksjonen analyserer en global variabel som kalles "errno" og gir en forklaring på feilen som har oppstått. For eksempel:

```C
if (fopen("filnavn.txt", "r") == NULL) {
    perror("Åpne filen");
}
```

Dette vil skrive ut en feilmelding hvis det oppstår en feil under åpningen av filen "filnavn.txt".

## Se også

- <https://www.programiz.com/c-programming/library-function/stdio.h/printf>
- <https://www.tutorialspoint.com/c_standard_library/stdio_h.htm>
- <https://www.gnu.org/software/libc/manual/pdf/libc.pdf>