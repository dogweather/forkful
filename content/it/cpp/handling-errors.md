---
title:                "Gestione degli errori"
date:                  2024-01-26T00:49:27.909391-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestione degli errori"
programming_language: "C++"
category:             "C++"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/handling-errors.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Gestire gli errori significa pianificare per quando le cose vanno storte. È vitale perché aiuta ad evitare crash e rende il tuo software robusto e facile da usare.

## Come fare:
Ecco un blocco try-catch di base per gestire un'eccezione:

```cpp
#include <iostream>
#include <stdexcept>

int main() {
    try {
        throw std::runtime_error("Oops! Qualcosa è andato storto.");
    } catch (const std::exception& e) {
        std::cerr << "Errore: " << e.what() << std::endl;
    }
    return 0;
}
```

Output di esempio:
```
Errore: Oops! Qualcosa è andato storto.
```

## Approfondimento
C++ ha avuto la gestione degli errori sin dai suoi primi giorni. La forma più basilare era il controllo dei valori di ritorno. Se hai esperienza, ti ricordi i giorni pre-standard: C con le classi e il controllo manuale degli errori.

Poi sono arrivate le eccezioni con C++ per darci un modo strutturato di affrontare problemi inattesi. Un'eccezione viene lanciata con `throw` e intercettata con `try/catch`.

Due tipi di errori sorgono spesso: errori logici, come un calcolo errato, ed errori di runtime, come l'accesso a un indirizzo di memoria non valido. Le eccezioni sono ideali per gli errori di runtime. Per gli errori logici, è spesso meglio usare asserzioni o codici di errore.

C'è un dibattito in corso su eccezioni versus codici di errore. Le eccezioni possono essere più lente e possono portare a flussi di controllo complessi. I codici di errore, sebbene più veloci, possono rendere il codice ingombrante e più difficile da mantenere. È un compromesso, quindi conoscere il proprio caso d'uso è fondamentale.

C++17 ha introdotto `std::optional` e `std::variant`, che sono alternative alle eccezioni. Sono utili per funzioni che possono o non possono restituire un risultato valido.

La sicurezza delle eccezioni può essere un altro grattacapo. Si tratta delle garanzie che il tuo codice fornisce nonostante le eccezioni. Ci sono tre livelli: base, forte e nothrow. Più garanzie offri, più il tuo codice potrebbe essere complesso.

Considerazioni finali—la gestione degli errori è tanto arte quanto scienza. Modella come la tua applicazione sopravvive all'aperto. Non abusare delle eccezioni. Puntare a codice leggibile e mantenibile.

## Vedi anche
- [cppreference sulla gestione delle eccezioni](https://en.cppreference.com/w/cpp/language/exceptions)
- [Il punto di vista di Bjarne Stroustrup sulla gestione degli errori](http://www.stroustrup.com/except.pdf)
- [Linee guida del nucleo C++ sulle eccezioni](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Re-exceptions)