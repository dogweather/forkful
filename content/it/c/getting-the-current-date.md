---
title:                "C: Ottener la data corrente"
simple_title:         "Ottener la data corrente"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perchè 

La data corrente è un'informazione importante in molte applicazioni. Può essere utilizzata per mostrare la data di creazione di un file, registrare l'orario di un evento, o semplicemente per fornire un feedback all'utente sul momento in cui sta utilizzando l'applicazione.

## Come Fare 

Per ottenere la data corrente in linguaggio C, è necessario utilizzare le funzioni fornite dalla libreria di sistema `time.h`. La funzione `time` restituirà il numero di secondi trascorsi dal 1 gennaio 1970 a mezzanotte. Questo valore può essere convertito in una data leggibile utilizzando le funzioni `localtime` e `asctime`.

Ecco un esempio di codice che stampa la data corrente nel formato "Giorno/Mese/Anno":

```C
#include <stdio.h>
#include <time.h>

int main() {
    // ottieni il tempo corrente
    time_t currentTime = time(NULL);

    // converti in una struttura di tipo tm
    struct tm *ptm = localtime(&currentTime);

    // stampa la data nel formato "gg/mm/aaaa"
    printf("%02d/%02d/%04d\n", ptm->tm_mday, ptm->tm_mon + 1, ptm->tm_year + 1900);
    return 0;
}
```

L'output di questo codice, se eseguito il 24 agosto 2021, sarà:

```
24/08/2021
```

## Approfondimenti

Per ottenere un maggior controllo sulla formattazione della data, è possibile utilizzare la funzione `strftime`, che permette di definire il formato della data desiderato. Inoltre, se è necessario manipolare il tempo in modo più preciso, è possibile utilizzare altre funzioni della libreria `time.h` come `difftime` per ottenere la differenza di tempo tra due momenti specifici.

Inoltre, in alcune situazioni è possibile che la data corrente non sia sufficiente e sia necessario utilizzare un servizio esterno per ottenere una data più precisa e affidabile. In questi casi, è possibile utilizzare librerie di terze parti o API online per ottenere la data corrente.

## Vedi Anche 

- [Documentazione ufficiale della libreria di sistema `time.h`](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Esempi di utilizzo delle funzioni `time.h`](https://www.geeksforgeeks.org/c-program-print-current-day-date-time/)