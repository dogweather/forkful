---
title:    "C: Convertire una data in una stringa"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché convertire una data in una stringa?

In programmazione, spesso è necessario convertire una data in una stringa per poterla manipolare e visualizzare in modi diversi. Ad esempio, potresti voler stampare una data in un formato specifico o salvarla in un file di testo. La conversione da una data a una stringa rende più semplice questo processo, in quanto la stringa può essere manipolata e stampata facilmente.

## Come convertire una data in una stringa

Per convertire una data in una stringa in linguaggio C, è necessario utilizzare la funzione `strftime` della libreria `<time.h>`. Di seguito è riportato un codice di esempio che mostra come utilizzare questa funzione per ottenere una stringa con la data corrente nel formato "gg/mm/aaaa".

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Dichiarazione della variabile time_t per la data
    time_t data_corrente;
    
    // Utilizzo della funzione time per ottenere la data corrente
    time(&data_corrente);
    
    // Dichiarazione della stringa per la data
    char stringa_data[11];
    
    // Utilizzo della funzione strftime per convertire la data in una stringa nel formato desiderato
    strftime(stringa_data, 11, "%d/%m/%Y", localtime(&data_corrente));
    
    // Stampa della stringa con la data
    printf("%s", stringa_data);
    
    return 0;
}
```

L'output di questo programma dovrebbe essere "05/04/2021", a seconda di quando viene eseguito.

## Approfondimento sulla conversione di date in stringhe

La funzione `strftime` ha diverse opzioni per formattare la stringa della data in diversi modi. Ad esempio, è possibile includere l'ora e i minuti nella stringa utilizzando il formato "%H:%M". Inoltre, è possibile specificare il nome del mese o del giorno della settimana in diverse lingue, utilizzando le opzioni "%B" e "%A" rispettivamente.

È importante notare che il formato della stringa finale dipende dal sistema operativo utilizzato. Ad esempio, in ambiente Windows il formato della data è diverso da quello di Linux. Si consiglia di consultare la documentazione ufficiale della funzione `strftime` per maggiori informazioni.

## Vedi anche

- [Documentazione ufficiale della funzione strftime (in inglese)](https://www.cplusplus.com/reference/ctime/strftime/)
- [Tutorial su come utilizzare la libreria <time.h> (in italiano)](https://variebld.online/blog/come-usare-la-libreria-time-h-in-c/)
- [Esempio di formattazione di una data in una stringa su Windows (in inglese)](https://stackoverflow.com/questions/1013860/how-to-convert-time-t-to-string-in-c/1013875#1013875)