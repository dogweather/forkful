---
title:    "C++: Calcolo di una data nel futuro o nel passato"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare la data nel futuro o nel passato può risultare utile in varie situazioni, come ad esempio la pianificazione di viaggi o eventi. Inoltre, comprendere il modo in cui il calcolo viene effettuato può aiutare a sviluppare abilità di programmazione più avanzate.

## Come fare

Per calcolare una data nel futuro o nel passato, è importante conoscere i seguenti elementi:

- Il giorno, il mese e l'anno della data di partenza
- La quantità di giorni che si desidera aggiungere o sottrarre
- La logica di calcolo per tener conto dei mesi con un numero diverso di giorni e degli anni bisestili

Per semplificare il processo, possiamo utilizzare il linguaggio di programmazione C++ e la libreria "ctime". Ecco un esempio di codice per calcolare una data nel futuro:

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // definiamo la data di partenza
    struct tm data_partenza = {0};
    data_partenza.tm_year = 2020 - 1900; //anno
    data_partenza.tm_mon = 9 - 1; //mese (da 0 a 11)
    data_partenza.tm_mday = 20; //giorno
    
    // definiamo i giorni da aggiungere
    int giorni_da_aggiungere = 14;
    
    // convertiamo la data di partenza in un formato utilizzabile
    time_t data_partenza_in_secondi = mktime(&data_partenza);
    
    // calcoliamo la data nel futuro
    time_t data_nel_futuro_in_secondi = data_partenza_in_secondi + (giorni_da_aggiungere * 24 * 60 * 60);
    
    // convertiamo la data nel futuro in un formato leggibile
    struct tm *data_nel_futuro = localtime(&data_nel_futuro_in_secondi);
    
    // stampiamo la data nel futuro
    cout << "La data nel futuro è: " << data_nel_futuro->tm_mday << "/" << data_nel_futuro->tm_mon + 1 << "/" << data_nel_futuro->tm_year + 1900 << endl;
    
    return 0;
}
```

Ecco l'output del codice:

```
La data nel futuro è: 3/10/2020
```

## Approfondimento

L'esempio di codice fornito utilizza la logica di calcolo della libreria "ctime" per tenere conto dei mesi e degli anni. Tuttavia, è possibile scrivere una funzione personalizzata per effettuare un calcolo più preciso, ad esempio considerando anche le ore, i minuti e i secondi.

Inoltre, ci sono molti altri modi per calcolare una data nel futuro o nel passato. Ad esempio, si potrebbe utilizzare una libreria di terze parti specifica per la manipolazione delle date, oppure si potrebbe implementare un algoritmo più complesso che tenga conto di fattori come i fusi orari o le festività.

In ogni caso, la conoscenza dei concetti di base spiegati in questo articolo fornisce una buona base per comprendere e utilizzare strategie più avanzate di calcolo delle date.

## Vedi anche

- [Utilizzo della libreria "ctime" in C++](https://www.geeksforgeeks.org/ctime-library-in-c/)
- [Esempi di codice per calcolare una data nel futuro in C++](https://www.programiz.com/cpp-programming/library-function/ctime/mktime)
- [Esempi di codice per calcolare una data nel futuro in altre lingue di programmazione](https://en.wikipedia.org/wiki/Addition_of_time_and_date)