---
title:                "C: Finn lengden av en streng"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##Hvorfor
Å finne lengden på en streng er en grunnleggende oppgave som ofte blir brukt i programmering. Det kan være nyttig å kjenne lengden på en streng når du for eksempel skal foreta en søkoperasjon eller når du trenger å begrense en brukers inndata.

##Slik gjør du det
Det finnes flere måter å finne lengden på en streng på i C-programmering, og her skal vi vise deg to eksempler. Først må du inkludere <string.h> biblioteket for å kunne bruke funksjonene som trengs.

```C
#include <stdio.h>
#include <string.h>

int main() {
  // Definer en streng
  char str[] = "Dette er en streng";

  // Eksempel 1: bruk av strlen() funksjonen
  int lengde = strlen(str);
  printf("Lengden på strengen er: %d\n", lengde);

  // Eksempel 2: bruk av sizeof() operatøren
  int lengde2 = sizeof(str) / sizeof(str[0]) - 1;
  printf("Lengden på strengen er: %d\n", lengde2);

  return 0;
}
```
**Utskrift:**

Lengden på strengen er: 19

Lengden på strengen er: 19

Som du kan se, kan du bruke enten strlen() funksjonen eller sizeof() operatøren for å finne lengden på en streng. Mens strlen() funksjonen returnerer antall tegn i en streng, returnerer sizeof() operatøren størrelsen på en variabel, som inkluderer \0 (null karakter) som blir brukt for å avslutte en streng.

##Dypdykk
En vanlig misforståelse når man skal finne lengden på en streng er å tro at det er det samme som størrelsen på en array. Dette er ikke tilfelle, ettersom størrelsen på en array inkluderer også plass til \0 (null karakter) mens lengden på en streng er antall tegn før \0.

Det er også viktig å merke seg at lengden på en streng ikke inkluderer selve \0 karakteren. For eksempel, en streng med ordet "Hei" vil ha en lengde på 3 siden det er tre tegn (H, e, i), men størrelsen på en array med denne strengen vil være 4, inkludert \0.

Når det gjelder funksjonen strlen(), så itererer den gjennom hele strengen til den når \0 karakteren og returnerer antall tegn den har iterert gjennom. Mens den er enklere å bruke, kan den være litt tregere enn å bruke sizeof() operatøren siden den faktisk må gå gjennom strengen.

##Se også
- [Offisiell dokumentasjon for strlen() funksjonen](https://www.cplusplus.com/reference/cstring/strlen/) (engelsk)
- [Offisiell dokumentasjon for sizeof() operatøren](https://www.cplusplus.com/reference/clibrary/cstring/strlen/) (engelsk)