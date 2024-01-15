---
title:                "Calcolare una data nel futuro o nel passato."
html_title:           "C++: Calcolare una data nel futuro o nel passato."
simple_title:         "Calcolare una data nel futuro o nel passato."
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perchè

Ci sono molte situazioni in cui potresti avere la necessità di calcolare una data nel futuro o nel passato usando il linguaggio di programmazione C++. Ad esempio, può essere utile per gestire le scadenze di progetti, per creare funzionalità dinamiche in un'applicazione o semplicemente per soddisfare la tua curiosità.

## Come Fare

Ecco un semplice esempio di come calcolare una data nel futuro o nel passato utilizzando C++:

```C++
#include <iostream>
#include <ctime>
using namespace std;

int main() {
    // Impostiamo la data attuale
    time_t now = time(0);
    // Convertiamo la data in una struttura di tipo tm
    tm* today = localtime(&now);

    // Calcoliamo una data nel futuro aggiungendo 10 giorni alla data attuale
    today->tm_mday += 10;
    // Convertiamo di nuovo la data in formato time_t
    time_t future = mktime(today);

    // Stampiamo la data futura
    cout << "Data nel futuro: " << ctime(&future) << endl;

    // Calcoliamo una data nel passato sottraendo 5 anni alla data attuale
    today->tm_year -= 5;
    // Convertiamo nuovamente la data in formato time_t
    time_t past = mktime(today);

    // Stampiamo la data passata
    cout << "Data nel passato: " << ctime(&past) << endl;
    return 0;
}
```
L'output di questo esempio sarà simile a questo:
```
Data nel futuro: Gio Nov 5 23:58:50 2020

Data nel passato: Sab Nov 5 23:58:50 2015
```

## Deep Dive

Ora che abbiamo visto un semplice esempio di come calcolare una data nel futuro o nel passato, è importante capire come funziona esattamente il processo. In C++, le date vengono rappresentate utilizzando la struttura `time_t`, che memorizza il numero di secondi che sono passati dal 1 gennaio 1970.

Per calcolare una data nel futuro o nel passato, ci avvaliamo delle funzioni `mktime` e `localtime`. La prima prende in input una struttura `tm` e la converte in un formato di tipo `time_t`, mentre la seconda ci permette di manipolare la data utilizzando i suoi campi, come ad esempio `tm_mday` per il giorno del mese o `tm_year` per l'anno.

## See Also
- [Documentazione di C++ su date e tempo](https://en.cppreference.com/w/cpp/chrono)
- [Tutorial su come lavorare con date in C++](https://www.cprogramming.com/tutorial/time.html)
- [Esempi di codice per calcolare date in C++](https://thispointer.com/find-out-current-datetime-in-c-windows-linux/)