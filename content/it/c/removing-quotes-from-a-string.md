---
title:                "Rimuovere le virgolette da una stringa"
date:                  2024-01-26T03:38:01.103320-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rimuovere le virgolette da una stringa"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Rimuovere le virgolette da una stringa significa eliminare eventuali segni di virgolettatura, sia che si tratti di virgolette singole ('') sia di virgolette doppie (""), che fanno parte del contenuto della stringa. I programmatori fanno ciò per sanificare l'input, preparare i dati per ulteriori elaborazioni o evitare errori di sintassi quando si lavora con percorsi di file e comandi in linguaggi che utilizzano le virgolette per delimitare le stringhe.

## Come:

Ecco una funzione in C che eliminerà quelle fastidiose virgolette dalle tue stringhe:

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
    char str[] = "Ha detto, \"Ciao, 'mondo'!\"";
    printf("Originale: %s\n", str);
    remove_quotes(str);
    printf("Sanificato: %s\n", str);
    return 0;
}
```

Output dell'esempio:

```
Originale: Ha detto, "Ciao, 'mondo'!"
Sanificato: Ha detto, Ciao, mondo!
```

## Approfondimento

Rimuovere le virgolette da una stringa è un compito che esiste dalla nascita della programmazione, in cui l'igiene dei dati era ed è ancora fondamentale per evitare errori (come attacchi di iniezione SQL) o per assicurarsi che una stringa possa essere passata in modo sicuro a sistemi che potrebbero confondere una virgoletta per un carattere di controllo.

Storicamente, i diversi linguaggi affrontano questo compito in modi diversi: alcuni dispongono di funzioni integrate (come `strip` in Python), mentre altri, come C, richiedono un'implementazione manuale a causa della sua attenzione a offrire ai sviluppatori un controllo di livello inferiore.

Le alternative includono l'utilizzo di funzioni di libreria come `strpbrk` per trovare le virgolette oppure l'impiego di espressioni regolari (con librerie come PCRE) per modelli più complessi, anche se ciò potrebbe essere eccessivo per il semplice compito di rimuovere le virgolette.

L'implementazione sopra si limita semplicemente a scandire ogni carattere nella stringa, copiando solo i caratteri non virgolettati nella posizione del puntatore di scrittura. Questo è efficiente perché viene eseguito sul posto senza la necessità di memoria aggiuntiva per la stringa risultante.

## Vedi Anche

- [Funzioni della Libreria Standard C](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - Espressioni regolari compatibili con Perl](https://www.pcre.org/)
- [Comprensione dei puntatori in C](https://www.learn-c.org/en/Pointers)
