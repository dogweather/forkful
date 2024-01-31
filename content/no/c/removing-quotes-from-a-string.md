---
title:                "Fjerne anførselstegn fra en streng"
date:                  2024-01-26T03:38:01.792456-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fjerne anførselstegn fra en streng"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å fjerne anførselstegn fra en streng betyr å strippe ut eventuelle anførselstegn—enten det er enkle ('') eller doble ("")—som er en del av strengens innhold. Programmerere gjør dette for å sanere inndata, forberede data for videre behandling, eller unngå syntaksfeil når de håndterer filbaner og kommandoer i språk som bruker anførselstegn for å markere strenger.

## Hvordan:

Her er en C-funksjon som vil skrubbe de irriterende anførselstegnene ut av strengene dine:

```c
#include <stdio.h>
#include <string.h>

void remove_quotes(char *str) {
    char *p_read = str, *p_write = str;
    while (*p_read) {
        if (*p_read != '"' && *p_read != '\'') {
            *p_write++ = *p_read;
        }
        p_read++;
    }
    *p_write = '\0';
}

int main() {
    char str[] = "He said, \"Hello, 'world'!\"";
    printf("Original: %s\n", str);
    remove_quotes(str);
    printf("Sanitisert: %s\n", str);
    return 0;
}
```

Eksempelutdata:

```
Original: He said, "Hello, 'world'!"
Sanitisert: He said, Hello, world!
```

## Dypdykk

Å fjerne anførselstegn fra en streng har vært en oppgave siden programmeringens daggry, der datahygiene var og fortsatt er nøkkelen til å unngå feil (som SQL-injeksjonsangrep) eller sørge for at en streng trygt kan overføres til systemer som kan forveksle et anførselstegn med et kontrolltegn.

Historisk sett håndterer forskjellige språk denne oppgaven forskjellig—noen har innebygde funksjoner (som `strip` i Python), mens andre, som C, krever manuell implementasjon på grunn av fokuset på å gi utviklere lavnivåkontroll.

Alternativer inkluderer å bruke bibliotekfunksjoner som `strpbrk` for å finne anførselstegn eller å benytte regulære uttrykk (med biblioteker som PCRE) for mer komplekse mønstre, selv om dette kan være overkill for å bare fjerne anførselstegn.

Implementeringen ovenfor skanner ganske enkelt gjennom hver karakter i strengen, og kopierer kun ikke-anførselstegn til skrivepekerens plassering. Dette er effektivt fordi det gjøres på stedet uten å trenge ekstra minne for resultatstrengen.

## Se Også

- [C Standard Library Functions](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - Perl Compatible Regular Expressions](https://www.pcre.org/)
- [Forståelse av Pekere i C](https://www.learn-c.org/en/Pointers)
