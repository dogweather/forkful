---
title:                "Capitalizzare una stringa"
html_title:           "C: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Capitalizzare una stringa è il processo di convertire tutte le lettere di una stringa in maiuscolo. I programmatori spesso lo fanno per uniformare una stringa o per confrontarla con altre stringhe.

## Come:
Un esempio di codice per capitalizzare una stringa in C è il seguente:
```
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main()
{
    char stringa[] = "esempio di stringa";
    char nuova_stringa[20];
    int i;

    printf("Stringa originale: %s\n", stringa);

    for(i = 0; i < strlen(stringa); i++)
    {
        nuova_stringa[i] = toupper(stringa[i]);
    }

    printf("Stringa capitalizzata: %s", nuova_stringa);

    return 0;
}
```

Questo codice utilizza la funzione `toupper()` della libreria `ctype.h` per convertire ogni lettera della stringa in maiuscolo e la funzione `strlen()` della libreria `string.h` per determinare la lunghezza della stringa originale. Il risultato della conversione viene salvato in una nuova stringa.

L'output di questo codice sarà:
```
Stringa originale: esempio di stringa
Stringa capitalizzata: ESEMPIO DI STRINGA
```

## Approfondimento:
La funzione `toupper()` viene utilizzata anche per convertire caratteri di un intero alfabeto, come ad esempio il turco. Inoltre, esistono anche altri metodi per capitalizzare una stringa, come ad esempio utilizzando il puntatore all'inizio della stringa e modificando i caratteri uno alla volta.

## Vedi anche:
Per ulteriori informazioni sulle funzioni utilizzate in questo articolo, si consiglia di consultare la documentazione ufficiale di C su `toupper()` e `strlen()`.