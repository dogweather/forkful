---
title:                "Ottenere la data corrente"
html_title:           "C++: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Ottenere la data attuale è un'operazione comune tra i programmatori, che serve ad inserire la data corretta in diverse applicazioni. Solitamente si utilizza questa funzionalità per registrare gli eventi, generare rapporti o per calcolare la durata tra due date.

## Come fare:
Per ottenere la data attuale in C++, è possibile utilizzare la libreria standard ```ctime```. Questa libreria fornisce la funzione ```std::time()``` che restituisce il numero di secondi trascorsi dal 1 gennaio 1970. Ecco un esempio di codice:

```C++
#include <iostream>
#include <ctime>

int main() {
  // Ottenere la data attuale
  std::time_t t = std::time(nullptr);

  // Convertire i secondi in una struttura tm
  struct std::tm * now = std::localtime(&t);

  // Stampare la data attuale
  std::cout << "La data attuale è: "
            << now->tm_mday << '/' << (now->tm_mon + 1) << '/' << (now->tm_year + 1900)
            << std::endl;

  return 0;
}
```

Output:
```
La data attuale è: 31/12/2021
```

## Approfondimento:
La funzione ```std::time()``` è stata introdotta nella libreria ```ctime``` nel C++11, ma esistono anche altre alternative per ottenere la data attuale, come ad esempio la libreria ```chrono```. Inoltre, è importante notare che la funzione ```std::time()``` restituisce i secondi trascorsi dal 1970 nell'orario locale, ma ci sono altre funzioni che permettono di ottenere la data in UTC.

## Vedi anche:
- [Documentazione della libreria ctime in C++](https://en.cppreference.com/w/cpp/chrono/c/strftime)
- [Tutorial sulla gestione della data in C++](https://www.geeksforgeeks.org/date-manipulation-c-c/)