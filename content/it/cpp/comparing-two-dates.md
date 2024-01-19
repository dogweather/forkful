---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Confrontare due date in C++ significa verificare se una data è maggiore, minore o uguale a un'altra. I programmatori lo fanno per gestire eventi a tempo, come la scadenza di una certa funzione.

## Come fare:

Ecco un esempio di come fare in C++.

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    time_t now = time(0);
    tm* ltm = localtime(&now);

    int todayDay = ltm->tm_mday;
    int todayMonth = 1 + ltm->tm_mon;
    int todayYear = 1900 + ltm->tm_year;

    int otherDay = 20;
    int otherMonth = 7;
    int otherYear = 2022;

    if ((todayYear > otherYear) || (todayYear == otherYear && todayMonth > otherMonth) || (todayYear == otherYear && todayMonth == otherMonth && todayDay > otherDay)) {
        cout << "La data odierna è successiva alla data inserita.";
    }
    else if (todayYear == otherYear && todayMonth == otherMonth && todayDay == otherDay) {
        cout << "La data odierna è la stessa della data inserita.";
    } else {
        cout << "La data odierna è precedente alla data inserita.";
    }

    return 0;
}
```

## Più in profondità

- **Contesto Storico**: Confrontare due date è un'operazione antica quanto la programmazione stessa. Siccome C++ non fornisce nativamente operazioni di confronto tra date, è necessario fare ciò manualmente.

- **Alternative**: Un'altra libreria utilizzata spesso per gestire date in C++ è Boost.Date_Time.

- **Dettagli Implementativi**: Il codice estrae l'anno, il mese e il giorno dalle due date e li confronta in sequenza. Se l'anno della data corrente è maggiore (o il mese/giorno nel caso l'anno sia uguale), allora la data corrente è successiva alla data inserita.

## Vedi Anche

- [La documentazione ufficiale del C++ sulle date](https://en.cppreference.com/w/cpp/chrono).
- [Libreria Boost Date_Time](https://www.boost.org/doc/libs/1_73_0/doc/html/date_time.html) per una gestione più sofisticata delle date.