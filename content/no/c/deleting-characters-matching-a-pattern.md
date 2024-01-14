---
title:    "C: Slette tegn som matcher et mønster"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger i programmeringen må man slette karakterer som passer til et bestemt mønster. Dette kan være nyttig når man for eksempel ønsker å filtrere ut uønskede data eller rense en tekststreng. Å vite hvordan man kan utføre denne oppgaven kan være nyttig i en rekke situasjoner.

## Slik gjør du det

Coding eksempel:

```C
#include <stdio.h>
#include <string.h>

int main() {
  // Definerer en tekststreng og et mønster
  char string[] = "Hei! Velkommen til min bloggpost.";
  char pattern[] = "!";
  int i, j;

  // Går gjennom hver karakter i tekststrengen
  for (i = 0; string[i] != '\0';) {
    int flag = 0;

    // Sjekker om nåværende karakter passer til mønsteret
    for (j = 0; pattern[j] != '\0'; j++) {
      if (string[i + j] != pattern[j]) {
        flag = 1;
        break;
      }
    }

    // Hvis mønsteret matcher, slett karakteren
    if (flag == 0) {
      for (j = i; string[j] != '\0'; j++) {
        string[j] = string[j + 1];
      }
    } else {
      i++;
    }
  }

  // Printer ut den endrede tekststrengen
  printf("%s\n", string);

  return 0;
}

```

Output:

```
Hei Velkommen til min bloggpost
```

## Dypdykk

Mens koden over viser en enkel løsning for å slette karakterer som matcher et mønster, kan det være verdt å se på mer avanserte metoder for å løse dette problemet. For eksempel kan man bruke innebygde funksjoner som `strchr` eller `strrchr` fra string.h biblioteket for å finne og slette et spesifikt tegn i en tekststreng.

En annen ting å vurdere er hvordan man håndterer store tekstfiler. I stedet for å lese og behandle hele filen på en gang, kan man implementere en metode for å lese data bit for bit. På denne måten kan man håndtere store filer mer effektivt og unngå å overskride minnebegrensninger.

## Se også

- [strchr og strrchr funksjoner i C](https://www.geeksforgeeks.org/strchr-function-in-c/)
- [Håndtering av store filer i C](https://www.geeksforgeeks.org/c-programming-for-handling-large-data-sets/)