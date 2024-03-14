---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:49.423219-07:00
description: "Le espressioni regolari (regex) offrono un modo per cercare, corrispondere\
  \ e manipolare stringhe utilizzando schemi definiti. I programmatori le usano\u2026"
lastmod: '2024-03-13T22:44:43.895613-06:00'
model: gpt-4-0125-preview
summary: "Le espressioni regolari (regex) offrono un modo per cercare, corrispondere\
  \ e manipolare stringhe utilizzando schemi definiti. I programmatori le usano\u2026"
title: Utilizzo delle espressioni regolari
---

{{< edit_this_page >}}

## Cosa & Perché?

Le espressioni regolari (regex) offrono un modo per cercare, corrispondere e manipolare stringhe utilizzando schemi definiti. I programmatori le usano estensivamente per compiti come la validazione degli input, l'analisi dei dati testuali e la ricerca di modelli all'interno di grandi file di testo, rendendole uno strumento potente in ogni linguaggio, inclusa la programmazione in C.

## Come fare:

Per utilizzare le espressioni regolari in C, lavorerai principalmente con la libreria regex POSIX (`<regex.h>`). Questo esempio dimostra una corrispondenza di base:

```c
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main(){
    regex_t regex;
    int return_value;
    char *pattern = "^a[[:alnum:]]"; // Schema per corrispondere stringhe che iniziano con 'a' seguito da caratteri alfanumerici
    char *test_string = "apple123";

    // Compila l'espressione regolare
    return_value = regcomp(&regex, pattern, REG_EXTENDED);
    if (return_value) {
        printf("Impossibile compilare la regex\n");
        exit(1);
    }

    // Esegue l'espressione regolare
    return_value = regexec(&regex, test_string, 0, NULL, 0);
    if (!return_value) {
        printf("Corrispondenza trovata\n");
    } else if (return_value == REG_NOMATCH) {
        printf("Nessuna corrispondenza trovata\n");
    } else {
        printf("Fallimento nella corrispondenza della regex\n");
        exit(1);
    }

    // Libera la memoria allocata usata dalla regex
    regfree(&regex);

    return 0;
}
```

Esempio di output per una stringa corrispondente ("apple123"):
```
Corrispondenza trovata
```
E per una stringa non corrispondente ("banana"):
```
Nessuna corrispondenza trovata
```

## Approfondimento:

Le espressioni regolari in C, come parte dello standard POSIX, offrono un modo robusto per eseguire corrispondenze e manipolazioni di stringhe. Tuttavia, l'API della libreria regex POSIX in C è considerata più macchinosa rispetto a quelle trovate in linguaggi progettati con funzionalità di manipolazione delle stringhe di prima classe come Python o Perl. La sintassi per gli schemi è simile tra i linguaggi, ma C richiede la gestione manuale della memoria e più codice per preparare, eseguire e pulire dopo l'uso degli schemi regex.

Nonostante queste sfide, imparare ad usare le regex in C è gratificante perché approfondisce la comprensione dei concetti di programmazione di basso livello. Inoltre, apre possibilità per la programmazione in C in aree come l'elaborazione del testo e l'estrazione dei dati dove le regex sono indispensabili. Per schemi più complessi o operazioni regex, alternative come la libreria PCRE (Perl Compatible Regular Expressions) potrebbero offrire un'interfaccia più ricca di funzionalità e in qualche modo più semplice, anche se richiede l'integrazione di una libreria esterna nel tuo progetto C.
