---
title:                "C++: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Spesso nella programmazione ci si trova a dover manipolare le date e, a volte, è necessario convertire una data in una stringa. Questo può risultare utile per la visualizzazione di date in formati specifici o per la memorizzazione dei dati in un file di testo.

In questo articolo, impareremo come convertire una data in una stringa utilizzando il linguaggio di programmazione C++.

## Come fare

Per convertire una data in una stringa utilizzando C++, è necessario utilizzare la libreria `chrono` e la funzione `put_time` della libreria `iomanip`. Iniziamo definendo un oggetto di tipo `std::chrono::system_clock::time_point` e inizializziamolo con la data da convertire.

```
#include <iostream>
#include <chrono>
#include <iomanip>

int main() {
  // Definizione della data da convertire - 25 aprile 2021 alle 18:30
  std::chrono::system_clock::time_point data_da_convertire = std::chrono::system_clock::from_time_t(1619383800);

  return 0;
}
```

Per convertire questa data in una stringa, utilizziamo la funzione `std::put_time()` e specificammo il formato desiderato. Ad esempio, se vogliamo ottenere una stringa nel formato "g/m/aaaa hh:mm" (giorno/mese/anno ora:minuti), possiamo scrivere il seguente codice:

```
#include <iostream>
#include <chrono>
#include <iomanip>

int main() {
  std::chrono::system_clock::time_point data_da_convertire = std::chrono::system_clock::from_time_t(1619383800);

  // Converte la data in una stringa nel formato "g/m/aaaa hh:mm"
  std::cout << "Data convertita = " << std::put_time(&data_da_convertire, "%d/%m/%Y %H:%M") << std::endl;

  return 0;
}
```

L'output di questo codice sarà "Data convertita = 25/04/2021 18:30".

## Deep Dive

La funzione `std::put_time()` utilizza il formato di stampa di `strftime()` della libreria C, quindi è possibile utilizzare gli stessi specificatori di formato. Ad esempio, potremmo scrivere un codice per ottenere una stringa nel formato "Giorno Settimana, DD MMMM YYYY" (es. Martedì, 07 Settembre 2021) utilizzando il seguente codice:

```
#include <iostream>
#include <chrono>
#include <iomanip>

int main() {
  std::chrono::system_clock::time_point data_da_convertire = std::chrono::system_clock::from_time_t(1630985400);
  
  // Converte la data in una stringa nel formato "Giorno Settimana, DD MMMM YYYY"
  std::cout << "Data convertita = " << std::put_time(&data_da_convertire, "%A, %d %B %Y") << std::endl;

  return 0;
}
```

L'output di questo codice sarà "Data convertita = Martedì, 07 Settembre 2021".

## Vedi anche

- [Documentazione ufficiale di std::chrono](https://en.cppreference.com/w/cpp/chrono) 
- [Documentazione ufficiale di std::put_time](https://en.cppreference.com/w/cpp/io/manip/put_time)