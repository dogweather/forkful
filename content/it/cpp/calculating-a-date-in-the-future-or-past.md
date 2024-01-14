---
title:    "C++: Calcolare una data nel futuro o nel passato"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile in varie situazioni, come nel campo della programmazione per gestire lo scorrere del tempo o per pianificare eventi futuri.

## Come fare

Per calcolare una data nel futuro o nel passato, è necessario utilizzare una combinazione di funzioni e operatori disponibili in C++. Ad esempio, possiamo utilizzare la funzione `time()` per ottenere l'ora corrente in secondi dal 1 gennaio 1970 e la struttura `tm` per rappresentare una data. Ecco un esempio di codice che calcola la data tra 14 giorni a partire da oggi:

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    time_t rawtime;
    struct tm * timeinfo;
    time(&rawtime);
    timeinfo = localtime(&rawtime);

    // aggiungo 14 giorni alla data corrente
    timeinfo->tm_mday += 14;
    
    // converto in secondi
    rawtime = mktime(timeinfo);

    // stampo la nuova data
    cout << "La data tra 14 giorni sarà: " << ctime(&rawtime) << endl;

    return 0;
}
```

Output:

```
La data tra 14 giorni sarà: Thu Nov 4 04:20:12 2021
```

## Approfondimento

Per calcolare una data nel futuro o nel passato, è importante comprendere il funzionamento della funzione `time()` e della struttura `tm`. Inoltre, è possibile utilizzare anche altre funzioni come `strftime()` per formattare la data in modo personalizzato o `difftime()` per calcolare la differenza in secondi tra due date.

Un altro aspetto da tenere in considerazione è la gestione dei fusi orari, che può influire sui calcoli delle date. È importante utilizzare le funzioni disponibili per la gestione dei fusi orari, come `gmtime()` o `localtime()`, in base alle esigenze del progetto.

## Vedi anche
- [Funzione `time()` su cplusplus.com](http://www.cplusplus.com/reference/ctime/time/)
- [Struttura `tm` su cplusplus.com](http://www.cplusplus.com/reference/ctime/tm/)
- [Funzione `strftime()` su cplusplus.com](http://www.cplusplus.com/reference/ctime/strftime/)
- [Funzione `difftime()` su cplusplus.com](http://www.cplusplus.com/reference/ctime/difftime/)
- [Gestione dei fusi orari su cplusplus.com](http://www.cplusplus.com/reference/ctime/timezone/)