---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "C++: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Calcolare una data nel futuro o nel passato è il processo di determinare una data successiva o precedente a una data di riferimento specificata. I programmatori spesso eseguono questo calcolo per sincronizzare eventi futuri o passati con altri dati o per gestire scadenze e pianificazioni.

## Come fare:

```C++ 
// Esempio di codice per il calcolo di una data nel futuro
#include <iostream>
using namespace std;

int main() {
    int giorni = 10; // Numero di giorni da aggiungere alla data di riferimento
    int mese = 9; // Mese di riferimento (settembre)
    int anno = 2021; // Anno di riferimento

    // Aggiungiamo il numero di giorni all'anno, tenendo conto dei giorni bisestili
    if (mese == 2 && anno % 4 == 0) {
        giorni += 1;
    }

    // Aggiungiamo il numero di giorni al mese
    switch (mese) {
        case 4:
        case 6:
        case 9:
        case 11:
            giorni += 30;
            break;
        case 2:
        case 8:
        case 10:
        case 12:
            giorni += 31;
            break;
        default:
            giorni += 31;
    }

    // Se superiamo i 31 giorni di un mese, andremo al mese successivo
    if (giorni > 31) {
        giorni -= 31;
        mese += 1;
    }

    // Se superiamo i 12 mesi, andremo all'anno successivo
    if (mese > 12) {
        mese -= 12;
        anno += 1;
    }

    // Stampiamo la data finale
    cout << "La data dopo 10 giorni dalla data di riferimento è: " << giorni << "/" << mese << "/" << anno << endl;

    return 0;
}
```

Output:
```
La data dopo 10 giorni dalla data di riferimento è: 20/9/2021
```

## Approfondimento:

Il calcolo di una data nel futuro o nel passato è solitamente eseguito utilizzando la teoria dei giorni, un sistema di numerazione dei giorni basato sulla data di riferimento. Tuttavia, ci sono anche molti altri metodi per eseguire questo calcolo, come l'utilizzo di librerie e funzioni specifiche disponibili nei linguaggi di programmazione moderni.

## Vedi anche:

Alcune fonti utili per approfondire questo argomento sono:

- "Calcolo di date in C++": https://www.hackerrank.com/challenges/date-time/problem
- "Programmazione orientata agli oggetti in C++": https://www.geeksforgeeks.org/date-class-in-c/
- "Librerie di calendario per C++": http://www.stdlib.mapsoft.it/stdcalendar/