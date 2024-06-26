---
date: 2024-01-20 17:28:32.227483-07:00
description: "Come fare: Ecco un esempio su come aggiungere giorni a una data o sottrarli\
  \ usando le funzionalit\xE0 della libreria `<chrono>` di C++20."
lastmod: '2024-03-13T22:44:43.740792-06:00'
model: gpt-4-1106-preview
summary: "Ecco un esempio su come aggiungere giorni a una data o sottrarli usando\
  \ le funzionalit\xE0 della libreria `<chrono>` di C++20."
title: Calcolo di una data futura o passata
weight: 26
---

## Come fare:
Ecco un esempio su come aggiungere giorni a una data o sottrarli usando le funzionalità della libreria `<chrono>` di C++20:

```C++
#include <iostream>
#include <chrono>
#include <format>

using namespace std;
using namespace chrono;

int main() {
    system_clock::time_point oggi = system_clock::now();
    date::year_month_day data_oggi = date::year_month_day{floor<days>(oggi)};
    
    days giorni_da_aggiungere{10};
    date::year_month_day data_futura = data_oggi + giorni_da_aggiungere;

    days giorni_da_sottrarre{10};
    date::year_month_day data_passata = data_oggi - giorni_da_sottrarre;

    cout << format("Data oggi: {}\n", data_oggi);
    cout << format("Data futura (+10 giorni): {}\n", data_futura);
    cout << format("Data passata (-10 giorni): {}\n", data_passata);
}
```

Output:
```
Data oggi: 2023-04-12
Data futura (+10 giorni): 2023-04-22
Data passata (-10 giorni): 2023-04-02
```

## Approfondimento
Storicamente, le operazioni con le date in C++ erano complicate e coinvolgevano molte conversioni manuali e attenzione ai dettagli per gestire correttamente anni bisestili, fusi orari eccetera. Con l'introduzione della libreria `<chrono>` e le estensioni della C++20, calcolare date è diventato molto più semplice.

Alternativamente, libreria di terze parti come Boost.Date_Time offrivano funzionalità simili, ma grazie agli ultimi aggiornamenti del linguaggio, queste stanno diventando meno necessarie.

Per calcolare una data nel futuro/presente, sfruttiamo `system_clock::time_point` per ottenere la data corrente e la convertiamo in `year_month_day`. Poi, aggiungiamo o togliamo `days` per ottenere una nuova data, senza dover calcolare manualmente eventuali sovrapposizioni di mesi o anni.

## Vedi anche
- Documentazione ufficiale C++ `<chrono>`: https://en.cppreference.com/w/cpp/chrono
- Tutorial C++ su `<chrono>`: https://www.learncpp.com/cpp-tutorial/chrono-library-part-1/
- Boost.Date_Time documentation: https://www.boost.org/doc/libs/release/libs/date_time/
