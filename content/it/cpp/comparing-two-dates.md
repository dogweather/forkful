---
title:                "C++: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché confrontare due date in C++

Confrontare due date è una delle operazioni più comuni in programmazione. Ciò può essere utile per verificare la validità di input utente, creare logiche di flusso o semplicemente per eseguire controlli di validità in generale. In questo post, vedremo come confrontare due date utilizzando il linguaggio di programmazione C++.

## Come fare

Per realizzare la comparazione delle date in C++, utilizzeremo l'oggetto `std::tm` della libreria standard di C++. Questo oggetto rappresenta una struttura temporale composta da elementi come il giorno, il mese, l'anno, l'ora, i minuti e i secondi. 

Per confrontare due date, è necessario prima di tutto convertire le date in oggetti `std::tm`. Ciò può essere fatto con l'aiuto della funzione `std::mktime`, che converte una struttura `std::tm` in un valore di tempo di tipo `time_t`. 

Una volta che abbiamo convertito le due date in oggetti `time_t`, possiamo semplicemente confrontarle utilizzando gli operatori di confronto standard di C++, come ad esempio `>` o `<`. Il seguente codice mostra come confrontare due date rappresentate come stringhe:

```C++
#include <iostream>
#include <ctime>

int main() {
  // definiamo due date come stringhe
  std::string date1 = "01/01/2021";
  std::string date2 = "15/06/2021";

  // convertiamo le date in oggetti std::tm
  std::tm tm1 = {};
  std::tm tm2 = {};

  strptime(date1.c_str(), "%d/%m/%Y", &tm1);
  strptime(date2.c_str(), "%d/%m/%Y", &tm2);

  // convertiamo le date in time_t
  time_t t1 = mktime(&tm1);
  time_t t2 = mktime(&tm2);

  // confrontiamo le date utilizzando gli operatori <
  if (t1 < t2) {
    std::cout << date1 << " viene prima di " << date2 << std::endl;
  }
  else {
    std::cout << date2 << " viene prima di " << date1 << std::endl;
  }

  return 0;
}
```

In questo esempio, utilizziamo la funzione `strptime` per convertire le date dal formato stringa al formato `std::tm`. In seguito, utilizziamo la funzione `mktime` per ottenere i valori di tempo corrispondenti alle date, che poi confrontiamo utilizzando l'operatore `<`. 

L'output del codice sopra riportato sarà:

```
01/01/2021 viene prima di 15/06/2021
```

## Approfondimento

Esistono diverse librerie esterne che possono semplificare la comparazione delle date in C++, come ad esempio Chrono. Questa libreria offre una maggiore precisione e flessibilità nella gestione del tempo e delle date.

Inoltre, è importante tenere presente che la funzione `mktime` converte le date in base alla timezone del sistema operativo. Ciò può causare problemi in caso di cambi di timezone o quando si lavora con date storiche. In questi casi, può essere più utile utilizzare la libreria Boost.Date_Time, che gestisce le date in modo indipendente dalla timezone del sistema.

## Vedi anche

- [Funzione std::mktime - cppreference.com](https://en.cppreference.com/w/cpp/chrono/mktime)
- [Libreria Chrono - cppreference.com](https://en.cppreference.com/w/cpp/chrono)
- [Libreria Boost.Date_Time - boost.org](https://www.boost.org/doc/html/date_time.html)