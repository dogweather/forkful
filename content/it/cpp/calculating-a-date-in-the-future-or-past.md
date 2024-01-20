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

# Calcolo delle Date Future e Passate con C++ 

## Che Cosa & Perché?
Calcolare una data futura o passata significa determinare una data specifica avanzando o arretrando un determinato numero di giorni da una data data. I programmatori lo fanno per gestire eventi programmati, calcolare scadenze, sommare giorni lavorativi e molte altre operazioni relative al tempo.

## Come Fare:
Per calcolare una data futura o passata in C++, l'API `<chrono>` fornisce funzioni per manipolare date e durate di tempo.

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    using namespace std;
    using namespace std::chrono;

    system_clock::time_point today = system_clock::now();

    // Aggiungere 30 giorni alla data corrente
    system_clock::time_point future_date = today + days(30);

    // Trasforma la data in un formato visibile all'utente
    time_t tt = system_clock::to_time_t(future_date);
    cout << "Data futura: " << ctime(&tt) << endl;

    return 0;
}
```

Ecco il possibile output di esempio:

```C++
Data futura: Sun Dec 10 15:38:34 2023
```

## Approfondimento:
La manipolazione delle date è un'operazione comune in programmazione, ma non è sempre così semplice come potrebbe sembrare. Le sfide includono la gestione degli anni bisestili, il cambio dell'ora legale, diverse metodologie per contare il tempo in diverse culture, ecc. 

In passato, i programmatori dovevano gestire questi problemi manualmente. Tuttavia, moderni linguaggi, come C++, hanno biblioteche che facilitano il calcolo delle date. 

Rispetto ad altre strategie per le operazioni con date, usare `<chrono>` in C++ ha i suoi vantaggi. Essa è una libreria standard, ben documentata e ampiamente supportata. Inoltre, gestisce numerose sfide della manipolazione del tempo automaticamente.

Tuttavia, per operazioni più complesse come la gestione del calendario, potrebbe essere necessario utilizzare librerie di terze parti come Boost Date_Time o C++ Date.

## Vedi Anche:
- Documentazione ufficiale di `<chrono>`: https://en.cppreference.com/w/cpp/chrono
- Guida alla programmazione della data e dell'ora in C++: https://www.learncpp.com/cpp-tutorial/date-times/
- Libreria Boost Date_Time: https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html
- Libreria C++ Date di Howard Hinnant: https://github.com/HowardHinnant/date