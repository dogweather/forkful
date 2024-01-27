---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
html_title:           "Bash: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Capitalizzare una stringa significa convertire ogni lettera minuscola in maiuscola. Programmers do it to standardize text input or make titles and headers more visible.

## Come fare:
```C
#include <stdio.h>
#include <ctype.h>

void capitalize(char *str) {
    while (*str) {
        *str = toupper((unsigned char) *str);
        str++;
    }
}

int main() {
    char testo[] = "ciao mondo!";
    capitalize(testo);
    printf("Testo capitalizzato: %s\n", testo);
    return 0;
}
```
Output:
```
Testo capitalizzato: CIAO MONDO!
```

## Approfondimento
Capitalizzare una stringa è un'esigenza comune nata quando i computer hanno cominciato a elaborare testi. Storicamente, la funzione `toupper` esiste da quando lo standard ANSI C è stato definito nel 1989. Alternatives include creating a custom function or using libraries like `string.h`. Dettagli implementativi ricordano che `toupper` funziona con singoli caratteri. Per gestire l'intera stringa, iteriamo su ogni carattere.

## Vedere anche
- Standard C library (ISO/IEC 9899:2018): http://www.open-std.org/jtc1/sc22/wg14/
- Documentazione su funzioni di carattere in C: https://en.cppreference.com/w/c/string/byte
- Stack Overflow discussions on string manipulation in C: https://stackoverflow.com/questions/tagged/c+strings
