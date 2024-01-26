---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:48:30.972624-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
("Hva & Hvorfor?")
Å generere tilfeldige tall betyr å skape tall som ikke kan forutsies. I programmering bruker vi dette for å tilføre tilfeldighet i spill, simuleringer og sikkerhet.

## How to:
("Slik gjør du:")
I C kan du generere pseudotilfeldige tall med `rand()` funksjonen, som krever `stdlib.h`. Men husk å så "frøet" med `srand()` for variasjon.

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    srand(time(NULL));  // Sår frøet med nåværende tid
    for(int i = 0; i < 5; i++) {
        printf("%d\n", rand() % 100);  // Tilfeldige tall mellom 0 og 99
    }
    return 0;
}
```

Sample output kan variere hver gang. Her er ett eksempel:

```
42
55
19
83
67
```

## Deep Dive:
("Dypdykk:")
Før `rand()` og `srand()`, var tilfeldighet vanskelig i datamaskiner. Maskiner er deterministiske, så matematisk tilfeldighet som vi bruker i dag er egentlig "pseudotilfeldig". C's `rand()` funksjon følger en forutsigbar sekvens, som er hvorfor vi bruker `srand()` for å gi et unikt frø, vanligvis tid. Det er mer avanserte metoder som `random()` og `arc4random()` på noen plattformer, eller til og med hardware baserte løsninger for større tilfeldighet.

Alternativer til `rand()` inkluderer funksjoner fra C11 standard biblioteket `stdrandom.h` som tilbyr bedre algoritmer og distribusjoner. Denne praksis, sammen med kryptografisk sikre biblioteker, forbedrer både tilfeldighet og sikkerhet.

## See Also:
("Se også:")
- C Standards documentation (https://en.cppreference.com/w/c/numeric/random)
- An Introduction to Random Number Generators in C (https://www.thesprucecrafts.com/random-numbers-in-c-4112003)
- C Programming/C Reference/stdlib.h/rand (https://en.wikibooks.org/wiki/C_Programming/C_Reference/stdlib.h/rand)
