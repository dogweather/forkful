---
title:                "Conversione di una data in una stringa"
html_title:           "C++: Conversione di una data in una stringa"
simple_title:         "Conversione di una data in una stringa"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui è necessario convertire una data in una stringa nel linguaggio di programmazione C++, ad esempio quando si vuole visualizzare la data in un formato personalizzato o quando si desidera salvare una data in un file di testo per scopi di archiviazione.

## Come fare

Ecco un esempio di codice che illustra come convertire una data in una stringa utilizzando le funzioni built-in del C++:

```C++
#include <iostream>
#include <ctime>
#include <string>

using namespace std;

int main() {
   // Inizializza una variabile di tipo time_t con la data e ora correnti
   time_t now = time(0);
   
   // Utilizza la funzione localtime per convertire la data e ora in una struttura tm
   tm *ltm = localtime(&now);
   
   // Utilizza la funzione string stream per creare una stringa con la data nel formato desiderato
   stringstream ss;
   ss << ltm->tm_mday << "/" << 1 + ltm->tm_mon << "/" << 1900 + ltm->tm_year;
   string data = ss.str();
   
   // Stampa la stringa con la data
   cout << "La data di oggi è: " << data << endl;
   
   return 0;
}
```

L'output di questo programma sarà qualcosa del genere:

```
La data di oggi è: 1/7/2020
```

In questo esempio, si utilizzano le funzioni `time`, `localtime` e `stringstream` per ottenere la data attuale in un formato leggibile. Si può facilmente personalizzare il formato della data modificando il codice all'interno della stringa di output del `stringstream`.

## Approfondimento

Ci sono diverse opzioni disponibili per convertire una data in una stringa nel linguaggio di programmazione C++. Una alternativa a quanto mostrato nell'esempio sopra è utilizzare le funzioni `strftime` o `put_time` per formattare la stringa con la data. Inoltre, esistono anche librerie di terze parti, come Boost Date Time e GNU Date and Time, che forniscono funzionalità avanzate per la gestione delle date e delle ore.

Altre considerazioni importanti per la conversione della data in una stringa includono il controllo degli errori e la gestione dei fusi orari. È importante assicurarsi che il codice sia in grado di gestire correttamente situazioni come anni bisestili e cambiamenti di fuso orario durante il calcolo della data.

## Vedi anche

- [Funzione time() di C++](https://en.cppreference.com/w/cpp/chrono/c/time)
- [Funzione localtime() di C++](https://en.cppreference.com/w/cpp/chrono/c/localtime)
- [Funzione stringstream di C++](https://en.cppreference.com/w/cpp/io/basic_stringstream)
- [Libreria Boost Date Time](https://www.boost.org/doc/libs/1_73_0/doc/html/date_time.html)
- [Libreria GNU Date and Time](https://www.gnu.org/software/libc/manual/html_node/Date-and-Time.html)