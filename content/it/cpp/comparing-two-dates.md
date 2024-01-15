---
title:                "Confrontare due date"
html_title:           "C++: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Quando si lavora con date in un programma, può essere necessario confrontare due diverse date per determinare quale è successa prima o se sono equivalenti. In questo articolo, esploreremo come confrontare due date in C++, fornendo esempi di codice e informazioni più approfondite sul processo.

## Come fare

Per confrontare due date in C++, puoi seguire questi semplici passaggi:

1. Includi la libreria `"chrono"` nel tuo programma, che contiene le funzioni e le strutture dati per lavorare con il tempo.
2. Definisci due oggetti di tipo `chrono::system_clock::time_point` che rappresentano le date che vuoi confrontare.
3. Utilizza l'operatore di confronto `==`, `!=`, `>`, `<`, `>=` o `<=` per confrontare le due date e determinare quale è precedente o se sono equivalenti.

Ecco un esempio completo di codice che confronta due date e stampa il risultato:

```C++
#include <iostream>
#include <chrono>

using namespace std;

int main() 
{
  chrono::system_clock::time_point date1 = chrono::system_clock::now(); //data corrente
  chrono::system_clock::time_point date2 = chrono::system_clock::now() + chrono::hours(24); //data di domani
  
  if (date1 == date2) 
  {
    cout << "Le due date sono uguali." << endl;
  }
  else if (date1 < date2) 
  {
    cout << "La prima data è precedente alla seconda." << endl;
  }
  else 
  {
    cout << "La seconda data è precedente alla prima." << endl;
  }

  return 0;
}
```

L'output di questo codice sarà:

```
La prima data è precedente alla seconda.
```

## Approfondimento

Oltre all'operatore di confronto, ci sono altri metodi per confrontare due date in C++, come ad esempio utilizzare la funzione `chrono::duration` per calcolare la differenza tra le due date in millisecondi, secondi, minuti, ore, giorni, ecc. Inoltre, se si vogliono confrontare le date con una maggiore precisione, è possibile utilizzare il metodo `chrono::duration::count()` per ottenere il numero di unità (ad esempio millisecondi) tra le due date. Per ulteriori informazioni su queste funzioni e metodi, si consiglia di consultare la documentazione ufficiale di C++.

## Vedi anche

- [Documentazione ufficiale di C++ su std::chrono](https://en.cppreference.com/w/cpp/chrono)
- [Esempi di confronto di date in C++](https://www.techiedelight.com/compare-date-time-cpp/)