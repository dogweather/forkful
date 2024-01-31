---
title:                "Utilizzo delle espressioni regolari"
date:                  2024-01-19
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
(## Cosa e Perché?)

Le espressioni regolari (regex) sono schemi usati per trovare corrispondenze o pattern in testi. Programmare con regex semplifica elaborare stringhe, validare input e manipolare dati.

## How to:
(## Come fare:)

Per usare le regex in C, possiamo utilizzare le funzioni contenute nella libreria `<regex.h>`. Ecco un esempio di base:

```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int reti;
    reti = regcomp(&regex, "^a[[:alnum:]]", 0);
    if (reti) {
        fprintf(stderr, "Non si può compilare l'espressione\n");
        return 1;
    }
    reti = regexec(&regex, "abc", 0, NULL, 0);
    if (!reti) {
        puts("Match trovato");
    } else if (reti == REG_NOMATCH) {
        puts("Nessun match");
    } else {
        fprintf(stderr, "Errore regex\n");
    }
    regfree(&regex);
    return 0;
}
```

Output:
```
Match trovato
```

## Deep Dive:
(## Approfondimento:)

Le regex in C si basano su POSIX, nate negli anni '80. Ci sono alternative come le librerie PCRE (Perl Compatible Regular Expressions) per maggiore flessibilità e feature. C implementa le regex tramite il tipo `regex_t` e funzioni come `regcomp()`, `regexec()`, e `regfree()`. Ricorda che lavorare con regex può portare a prestazioni basse su testi molto grandi.

## See Also:
(## Vedi Anche:)

- Documentazione di libreria `regex.h`: https://pubs.opengroup.org/onlinepubs/007908775/xsh/regex.h.html
- Tutorial su regex: https://www.regular-expressions.info/tutorial.html
- Documentazione PCRE: https://www.pcre.org/
