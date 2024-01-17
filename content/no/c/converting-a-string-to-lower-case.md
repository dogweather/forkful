---
title:                "Konvertere en streng til små bokstaver"
html_title:           "C: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Konvertering av en streng til små bokstaver er en vanlig oppgave for programmerere. Dette er nyttig for å sikre konsistens i data og for å sammenligne og behandle tekst på en enklere måte.

# Hvordan gjør man det:
Kodingseksempler for å konvertere en streng til små bokstaver i C:
```
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main(){
    char str[] = "Hvordan konvertere dette til små bokstaver?";
	
    // Ved å bruke en løkke og to innebygde funksjoner, kan vi enkelt konvertere strengen til små bokstaver
    for(int i = 0; i < strlen(str); i++){
        str[i] = tolower(str[i]);
    }

    printf("%s", str);

    return 0;
}
```

#### Output:
```
hvordan konvertere dette til små bokstaver?
```

# Dypdykk:
* **Historisk kontekst:** Konvertering av strenger til små bokstaver har blitt brukt i programmering siden de tidlige dagene på 1950-tallet, da ASCII-karaktersettet ble introdusert.
* **Alternativer:** Andre metoder for å konvertere strenger til små bokstaver inkluderer å bruke bibliotekfunksjoner som `strlwr()` og `tolower()`, samt å bruke maskininstruksjoner for å konvertere tegnene.
* **Implementasjonsdetaljer:** I C, brukes funksjonen `tolower()` fra `ctype.h`-biblioteket til å konvertere en streng til små bokstaver. Denne funksjonen tar hensyn til språkspesifikke regler, som i det greske alfabetet hvor noen bokstaver har en annen versjon for små bokstaver.

# Se også:
* [Konvertering av strenger til store bokstaver i C](https://www.programiz.com/c-programming/library-function/string.h/toupper)
* [ASCII-karaktersettet](https://www.asciitable.com/) 
* [C Programming: A Modern Approach](https://www.amazon.com/Programming-Modern-Approach-2nd-Pearson/dp/0393979504)