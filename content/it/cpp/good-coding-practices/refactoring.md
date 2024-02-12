---
title:                "Rifattorizzazione"
date:                  2024-01-26T01:17:04.364876-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rifattorizzazione"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/refactoring.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Il refactoring è il processo di modifica della struttura interna di un programma informatico senza alterarne il comportamento esterno. I programmatori lo fanno per pulire il loro codice, rendendolo più facile da capire, mantenere ed estendere.

## Come fare:

Immaginate di avere una funzione che fa un po' troppo, come questo metodo ingombrante che inizializza un oggetto ed esegue anche il logging:

```C++
#include <iostream>

class Widget {
public:
    void init(bool verbose) {
        // Logica di inizializzazione
        // ...

        // Logging dettagliato
        if (verbose) {
            std::cout << "Widget initialized!" << std::endl;
        }
    }
};

// Uso:
Widget w;
w.init(true);
```

Output:
```
Widget initialized!
```

Refactoring questo in metodi più puliti e focalizzati potrebbe apparire così:

```C++
#include <iostream>

class Widget {
public:
    void init() {
        // Solo logica di inizializzazione
        // ...
    }

    void logInitialization() const {
        std::cout << "Widget initialized!" << std::endl;
    }
};

// Uso:
Widget w;
w.init();
w.logInitialization();
```

Questo cambiamento non ha alterato ciò che fa il programma ma rende la classe `Widget` più modulare e il suo uso più chiaro.

## Approfondimento

Il concetto di refactoring, così come lo conosciamo oggi, ha le sue radici nelle comunità di programmazione Smalltalk degli anni '80 ed è stato fortemente popolarizzato dal libro di Martin Fowler "Refactoring: Improving the Design of Existing Code" del 1999. Oggi, il refactoring è una parte fondamentale dello sviluppo software moderno, integrato in varie metodologie di sviluppo come Agile e TDD (Test-Driven Development).

Quando parliamo di alternative al refactoring, ci addentriamo nel territorio della riscrittura o del ridisegno. Il refactoring è strategico e incrementale, mentre una riscrittura può eliminare il codice esistente a favore di una nuova soluzione. Il ridisegno, invece, può comportare cambiamenti più significativi incluso l'alterare la funzionalità, che è un obiettivo non previsto per il puro refactoring.

I dettagli implementativi sul refactoring possono diventare piuttosto granulari. Esistono molti "code smells" che potrebbero spingere a un refactoring, come metodi lunghi, classi grandi o codice duplicato. Esistono strumenti automatizzati che possono assistere nel refactoring, come "Clang-Tidy" per C++, che può individuare problemi ed applicare alcune correzioni.

Inoltre, il refactoring richiede una solida suite di test per garantire che la funzionalità rimanga inalterata. Senza test, si è essenzialmente alla cieca e si rischiano regressioni.

## Vedi Anche

Per una comprensione più approfondita del refactoring e per vedere altri esempi, potreste voler consultare:

- Il testo classico di Martin Fowler “Refactoring: Improving the Design of Existing Code” per idee fondamentali e strategie.
- La documentazione di `Clang-Tidy` su https://clang.llvm.org/extra/clang-tidy/ per il supporto al refactoring automatizzato in C++.
- “Working Effectively with Legacy Code” di Michael Feathers, che fornisce tecniche per il refactoring sicuro nel contesto di basi di codice esistenti meno che perfette.
