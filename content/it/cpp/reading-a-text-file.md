---
title:                "C++: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perchè 

Leggere un file di testo è un'operazione fondamentale nella programmazione in C++. Spesso abbiamo la necessità di leggere dati da un file per elaborarli o visualizzarli in qualche modo all'interno del nostro programma.

## Come farlo

Per leggere un file di testo in C++, dobbiamo seguire alcuni passaggi:

1. Aprire il file utilizzando la funzione `ifstream`
2. Controllare se il file è stato aperto correttamente
3. Leggere il contenuto del file e memorizzarlo in una variabile
4. Chiudere il file utilizzando la funzione `close()` 

Ecco un esempio di codice che mostra come leggere un file di testo contenente una lista di nomi e stamparla a schermo:

```
#include <iostream>
#include <fstream>

int main() 
{
    // apriamo il file di testo
    std::ifstream input("nomi.txt");

    // controlliamo se il file è stato aperto correttamente
    if (!input.is_open()) 
    {
        std::cout << "Errore durante l'apertura del file!";
        return 1;
    }

    // leggiamo il contenuto del file e lo memorizziamo in una variabile
    std::string nome;
    while (input >> nome) 
    {
        // stampiamo il nome a schermo
        std::cout << nome << std::endl;
    }

    // chiudiamo il file
    input.close();
    return 0;
}
```

Ecco il contenuto del file `nomi.txt`:

```
Marco
Luca
Giulia
Pietro
```

E questa è l'output del nostro programma:

```
Marco
Luca
Giulia
Pietro
```

## Approfondimento

Oltre alle operazioni di base per leggere un file di testo, esistono molte altre funzionalità utili che possono aiutarci nell'elaborazione dei dati. Ad esempio, possiamo leggere solo una parte del file utilizzando il metodo `getline()` oppure possiamo specificare il delimitatore per separare i dati durante la lettura.

La lettura di file di testo è solo uno degli aspetti dell'accesso ai file in C++. Per saperne di più, puoi consultare la documentazione ufficiale sulle operazioni con file del linguaggio.

## Vedi anche

- [C++: operazioni con file](https://en.cppreference.com/w/cpp/io)
- [Tutorial su come leggere e scrivere su file utilizzando C++](https://www.geeksforgeeks.org/basics-file-handling-c/)
- [Esempi di codice per la lettura di file in C++](https://www.programiz.com/cpp-programming/library-function/ofstream/ofstream-open)