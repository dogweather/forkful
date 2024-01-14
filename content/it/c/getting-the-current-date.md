---
title:                "C: Ottenere la data corrente"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Spesso, nei programmi che scriviamo, è importante avere accesso alla data corrente. Potremmo voler stamparla a schermo, utilizzarla per prendere decisioni all'interno del nostro codice o anche semplicemente utilizzarla come parte di un'operazione matematica. In questo post, impareremo come ottenere la data corrente utilizzando il linguaggio C.

## Come fare

Per ottenere la data corrente in C, possiamo utilizzare la funzione `time()` dalla libreria standard `time.h`. Questa funzione restituirà il numero di secondi trascorsi dal 1 gennaio 1970. Utilizzando la funzione `localtime()`, possiamo quindi convertire questo numero in una struttura `tm`, contenente informazioni sulla data e sull'ora. Qui di seguito un esempio di codice:

 ```C
#include <stdio.h>
#include <time.h>

int main() {
    // otteniamo il tempo corrente
    time_t currentTime = time(NULL);
    
    // convertiamo il tempo in una struttura tm
    struct tm* currentDateTime = localtime(&currentTime);
    
    // stampiamo la data
    printf("Oggi è il %d/%d/%d\n", currentDateTime->tm_mday, currentDateTime->tm_mon+1, currentDateTime->tm_year+1900);
    
    return 0;
}
```

L'output di questo codice sarà il seguente:

```
Oggi è il 25/02/2021
```

Ci sono anche diverse altre funzioni disponibili nella libreria `time.h` che ci permettono di manipolare e formattare ulteriormente la data, come ad esempio `strftime()` per convertire la struttura `tm` in una stringa formattata. Per ulteriori informazioni, si consiglia di consultare la documentazione ufficiale di C.

## Approfondimento

Sebbene l'uso della funzione `time()` sia semplice e funzionale per la maggior parte dei casi, è importante tenere presente che può anche essere soggetta ad alcuni errori. Ad esempio, se il sistema sperimenta un'interruzione di servizio nell'ora corrente, la funzione potrebbe restituire un valore non corretto. Inoltre, ci sono anche altri modi per ottenere la data corrente, come ad esempio utilizzando delle librerie esterne o richiamando comandi di shell.  

## Vedi anche

- [Documentazione ufficiale di C](https://devdocs.io/c/)
- [Tutorial su C su Programiz](https://www.programiz.com/c-programming)
- [Libreria standard time.h](https://www.tutorialspoint.com/c_standard_library/time_h.htm)