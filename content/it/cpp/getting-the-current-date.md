---
title:    "C++: Ottenere la data corrente"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Perché
Molti programmatori potrebbero chiedersi perché sia utile avere accesso alla data corrente durante la programmazione. In questo articolo esploreremo le motivazioni dietro questa esigenza e vedremo come è possibile ottenere la data corrente utilizzando il linguaggio di programmazione C++.

## Come fare
Per ottenere la data corrente, dobbiamo utilizzare una funzione della libreria standard di C++, chiamata `time()`. Questa funzione restituisce il numero di secondi trascorsi dal periodo di tempo noto come "epoch" (1 gennaio 1970 ore 00:00:00). Possiamo utilizzare questo valore per creare una data concreta.

```C++
#include <iostream>
#include <ctime>

int main() {
    // Otteniamo il valore dei secondi trascorsi dall'epoch
    std::time_t now = std::time(nullptr);
    // Convertiamo i secondi in una struttura di tipo tm
    std::tm *currentDate = std::localtime(&now);

    // Stampiamo la data corrente formattata utilizzando la struttura tm
    std::cout << "La data corrente è: "
              << currentDate->tm_mday << "/" << (currentDate->tm_mon + 1) << "/" << (currentDate->tm_year + 1900) << std::endl;
    
    return 0;
}
```

Questo codice utilizza la funzione `localtime()` per convertire il valore dei secondi in una struttura `tm` che contiene tutte le informazioni sulla data corrente, come giorno, mese e anno. Infine, utilizzando la sintassi `currentDate->tm_mday` possiamo accedere ai valori specifici e stamparli a schermo per ottenere una data formattata correttamente.

## Approfondimento
La funzione `time()` utilizzata precedentemente, restituisce il numero di secondi trascorsi dall'epoch in formato `time_t`, un tipo di dato a 32 o 64 bit a seconda dell'implementazione. La funzione `localtime()` trasforma questo valore in una data locale, basandosi sull'ora impostata nel sistema operativo.

Questa è una semplice implementazione per ottenere la data corrente, ma esistono anche molte librerie di terze parti che forniscono funzionalità più avanzate per lavorare con le date, come la gestione di fusi orari e la conversione tra formati differenti.

## Vedi anche
- [Documentazione di C++ sulla funzione `time()`](https://en.cppreference.com/w/cpp/chrono/c/time)
- [Altre opzioni per ottenere la data corrente in C++](https://www.geeksforgeeks.org/working-with-date-and-time-in-cpp/)
- [Libreria open source Boost per la gestione delle date in C++](https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html)