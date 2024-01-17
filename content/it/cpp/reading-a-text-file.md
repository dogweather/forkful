---
title:                "Leggere un file di testo"
html_title:           "C++: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

La lettura di un file di testo è semplicemente il processo di accedere alle informazioni contenute in un file di testo e utilizzarle in un programma. I programmatori lo fanno per accedere a dati importanti o per manipolare il contenuto del file in qualche modo.

## Come fare:

Ecco un semplice esempio di codice C++ che legge un file di testo e ne stampa il contenuto:

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
    // Apre il file di testo in modalità lettura
    ifstream file("testo.txt");

    // Se il file è stato aperto correttamente
    if (file.is_open()) {
        // Crea una variabile per contenere una riga del file
        string line;

        // Utilizza un ciclo per leggere ogni riga del file
        while (getline(file, line)) {
            // Stampa il contenuto della riga
            cout << line << endl;
        }

        // Chiude il file
        file.close();
    }
    else {
        // Output di errore se il file non può essere aperto
        cout << "Errore: impossibile aprire il file!" << endl;
    }
    return 0;
}
```

Esempio di contenuto del file "testo.txt":

```
Questo è un esempio di contenuto di un file di testo.
Puoi scrivere tutte le informazioni che vuoi all'interno di un file come questo.
Puoi anche utilizzare caratteri speciali o spazi bianchi.
```

Esempio di output:

```
Questo è un esempio di contenuto di un file di testo.
Puoi scrivere tutte le informazioni che vuoi all'interno di un file come questo.
Puoi anche utilizzare caratteri speciali o spazi bianchi.
```

## Approfondimento:

### Contesto storico:
La lettura di file di testo è stata utilizzata fin dai primi giorni della programmazione. Prima dell'utilizzo dei database, i file di testo erano l'unico modo per memorizzare dati importanti come informazioni di contabilità o rubriche telefoniche.

### Alternative:
Oltre alla lettura di file di testo, i programmatori possono anche utilizzare l'input da tastiera o la connessione a un database per ottenere i dati di cui hanno bisogno. Tuttavia, la lettura di file di testo è ancora ampiamente utilizzata per controllare la configurazione dei file di un programma o la gestione dei file di grandi dimensioni.

### Dettagli di implementazione:
Per leggere un file di testo, è necessario includere la libreria appropriata (```<fstream>``` nel codice sopra) e utilizzare le funzioni corrette per aprire, leggere e chiudere il file. Esistono anche librerie esterne che semplificano ulteriormente questo processo, come la libreria Boost C++.

## Vedi anche:

- [Manuale C++: Lettura di file di testo](https://www.cplusplus.com/doc/tutorial/files/)
- [Libreria Boost C++](https://www.boost.org/)