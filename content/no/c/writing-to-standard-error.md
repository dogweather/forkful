---
title:                "C: Skriver til standardfeil"
simple_title:         "Skriver til standardfeil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er en essensiell del av programmering å kunne skrive til standard error. Dette gjør det mulig å feilsøke og identifisere problemer under kjøring av programmet ditt. Det er også en måte å kommunisere til brukeren at noe ikke gikk som forventet.

## Hvordan

Å skrive til standard error i C er enkelt og kan gjøres ved å bruke funksjonen `fprintf()` med `stderr`-parameteren. La oss se på et eksempel:

```C
#include <stdio.h>

int main() {
  int num1, num2;

  printf("Skriv inn to tall: ");
  scanf("%d %d", &num1, &num2);

  if (num2 == 0) {
    fprintf(stderr, "Error: Kan ikke dele på null!\n");
    return 1;
  }

  printf("%d / %d = %d\n", num1, num2, num1 / num2);

  return 0;
}
```

Her bruker vi `fprintf()` til å skrive en feilmelding til standard error hvis brukeren prøver å dele på 0. Vi har også brukt `\n` for å legge til en linjeskift i meldingen. Kjøringen av dette programmet kan se slik ut:

```bash
$ ./divider
Skriv inn to tall: 10 2
10 / 2 = 5
$ ./divider
Skriv inn to tall: 10 0
Error: Kan ikke dele på null!
```

Som du ser blir feilmeldingen skrevet til standard error og ikke standard output.

## Dypdykk

Det er viktig å merke seg at standard error har en annen buffer enn standard output. Dette betyr at meldingene kan komme i en annen rekkefølge enn du forventer. Det er også mulig å omdirigere standard error til en fil hvis du ønsker å lagre feilmeldingene for senere bruk.

Du kan også bruke `fprintf()` til å skrive til standard output, men da må du bruke `stdout`-parameteren istedenfor `stderr`.

## Se også

- [Funksjonen `fprintf()` i C](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)
- [Feilsøking i C-programmer](https://www.tutorialspoint.com/cprogramming/c_debugging.htm)