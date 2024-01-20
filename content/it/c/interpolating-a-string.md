---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?

L'interpolazione della stringa è un modo attraverso il quale possiamo inserire variabili o espressioni all'interno di una stringa. I programmatori lo fanno per creare output più dinamici e per semplificare la concatenazione di stringhe.

## Come fare:

In C, generalmente usiamo la funzione `printf()` per l'interpolazione della stringa. Di seguito è riportato un esempio:

```C
#include <stdio.h>

int main() {
    int age = 22;
    printf("Ho %d anni.\n", age);
    return 0;
}
```

Il codice sopra stamperà: `Ho 22 anni.`. Qui, `%d` è un segnaposto che verrà sostituito dall'intero `age`.

## Approfondimento

Storicamente, l'interpolazione della stringa è stata molto utilizzata nelle versioni precedenti di molti linguaggi di programmazione, compreso C. Ad esempio, in Perl o Ruby, si possono facilmente interpolare le stringhe con l'uso del simbolo `#{}`.

Ci sono alternative all'interpolazione delle stringhe in C, come la concatenazione delle stringhe con `strcat()`, ma le stringhe concatenate in questo modo possono essere più difficili da leggere e gestire.

In C, l'interpolazione delle stringhe viene gestita principalmente da `printf()`, che richiede la conoscenza del tipo di dati che si desidera interpolare (ad esempio, `%d` per interi, `%f` per float, `%s` per stringhe).

## Leggi Anche

Per un approfondimento sull'interpolazione delle stringhe e sulla funzione `printf()`, visita i seguenti collegamenti:

- [C Programming/String usage](https://www.wikibooks.org/wiki/C_Programming/String_usage) su Wikibooks
- [What is Interpolation in PHP?](https://www.geeksforgeeks.org/what-is-variable-interpolation-in-php/) su GeeksforGeeks (anche se si riferisce a PHP, il concetto di interpolazione è lo stesso)