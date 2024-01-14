---
title:                "C: Konvertere en streng til små bokstaver."
simple_title:         "Konvertere en streng til små bokstaver."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Hvorfor
Det å konvertere en streng til små bokstaver kan være nyttig i mange ulike situasjoner, som for eksempel å sammenligne brukerinput mot en forventet verdi. Ved å gjøre dette, sikrer man at både store og små bokstaver vil bli akseptert som likeverdige.

# Hvordan
Det finnes flere ulike metoder for å konvertere en streng til små bokstaver i C-programmering. En vanlig metode er å iterere gjennom hver enkelt bokstav i strengen og deretter bruke den innebygde funksjonen "tolower" for å endre til små bokstaver.

```C
// Eksempel på konvertering av streng til små bokstaver ved hjelp av løkke og tolower-funksjonen
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main() {
    char streng[] = "Denne Strengen Vil Bli Konvertert";
    int lengde = strlen(streng);

    for (int i = 0; i < lengde; i++) {
        streng[i] = tolower(streng[i]);
    }

    printf("%s", streng);
    return 0;
}

// Output: denne strengen vil bli konvertert
```

En annen alternativ måte å oppnå det samme på er å bruke funksjonen "strlwr" fra header-filen "string.h".

```C
// Eksempel på konvertering av streng til små bokstaver ved hjelp av strlwr-funksjonen
#include <stdio.h>
#include <string.h>

int main() {
    char streng[] = "Denne Strengen Vil Bli Konvertert";

    printf("%s", strlwr(streng));
    return 0;
}

// Output: denne strengen vil bli konvertert
```

# Dypdykk
Ved konvertering av strenger til små bokstaver er det viktig å være oppmerksom på begrensningene. Dette inkluderer hvordan ulike språk håndterer ulike bokstaver, og at noen spesielle tegn ikke alltid vil bli konvertert riktig. Det er derfor viktig å teste og eventuelt justere kode for å sikre at konverteringen utføres som forventet.

# Se også
- [Strlwr funksjonen i C](https://www.tutorialspoint.com/c_standard_library/c_function_strlwr.htm)
- [Konvertere streng til små bokstaver i C](https://www.geeksforgeeks.org/convert-string-lowercase-string-c/)