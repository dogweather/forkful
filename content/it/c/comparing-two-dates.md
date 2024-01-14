---
title:                "C: Confrontare due date"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare due date è un'operazione comune quando si lavora con dati temporali in informatica. Questo può essere utile per calcolare differenze di tempo, o per verificare se una data è successiva a un'altra. In questo articolo vedremo come implementare questa funzionalità in linguaggio C.

## Come fare

Per confrontare due date in C, utilizzeremo la funzione "difftime" della libreria "time.h". Questa funzione restituisce la differenza di tempo in secondi tra due date. Di seguito è riportato un esempio di come utilizzare la funzione per confrontare due date:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Definire due date come strutture "tm"
    struct tm date1 = {0}, date2 = {0};
    
    // Impostare la prima data
    date1.tm_year = 120; // 2020
    date1.tm_mon = 6; // Luglio (mese 0-11)
    date1.tm_mday = 15;
    
    // Impostare la seconda data
    date2.tm_year = 120; // 2020
    date2.tm_mon = 6; // Luglio (mese 0-11)
    date2.tm_mday = 18;
    
    // Calcolare la differenza di tempo in secondi
    double diff = difftime(mktime(&date2), mktime(&date1));
    
    // Stampare il risultato
    printf("La differenza di tempo tra le due date è di %.0f secondi.", diff);
    
    return 0;
}
```

Output:

```
La differenza di tempo tra le due date è di 259200 secondi.
```

Nell'esempio sopra, abbiamo creato due strutture "tm" per rappresentare le date 15/07/2020 e 18/07/2020. Utilizzando la funzione "difftime" e la funzione "mktime" per convertire le strutture in oggetti di tipo "time_t", abbiamo calcolato la differenza di tempo tra le due date, che corrisponde a 3 giorni (259200 secondi).

## Approfondimento

Per una maggiore precisione, è possibile utilizzare la funzione "difftime" per confrontare anche l'ora, i minuti e i secondi delle due date. Inoltre, è importante notare che la funzione "mktime" tiene conto di eventuali fusi orari impostati sul sistema operativo. Ciò può influire sul risultato della differenza di tempo tra le date. 

Un'altra opzione per confrontare due date in C è utilizzare la funzione "difftime" insieme alla funzione "time", che restituisce l'ora attuale in formato "time_t". In questo modo, è possibile confrontare una data inserita manualmente con la data attuale.

## Vedi anche

- [Documentazione della funzione difftime in C](https://www.tutorialspoint.com/c_standard_library/c_function_difftime.htm)
- [Documentazione della libreria time.h in C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Come convertire una data in formato "time_t" in C](https://www.codingame.com/playgrounds/14213/how-to-convert-a-datestring-to-a-time_t-object-in-c/how-to-convert-a-datestring-to-a-time_t-object-in-c)