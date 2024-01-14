---
title:                "C++: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

La lettura degli argomenti della riga di comando è un'abilità fondamentale per ogni programmatore C++. Attraverso questa funzionalità, è possibile passare parametri al tuo programma direttamente dalla riga di comando, rendendolo più flessibile e personalizzabile.

## Come

Per leggere gli argomenti della riga di comando nel tuo programma C++, devi seguire alcuni semplici passaggi:

1. Includi la libreria <iostream> nel tuo codice per utilizzare le funzioni di input/output
2. Utilizza il parametro **argc** per ottenere il numero di argomenti passati dalla riga di comando
3. Utilizza il parametro **argv** per accedere alla lista di argomenti passati come array di stringhe

Ecco un breve esempio di codice che legge gli argomenti della riga di comando e stampa il loro contenuto:

```C++

#include <iostream>

using namespace std;

int main(int argc, char* argv[]) {

    // Controllo se sono stati passati degli argomenti
    if(argc > 1) {
        
        // Stampo ogni argomento uno per uno
        for(int i = 1; i < argc; i++) {
            cout << "Argomento " << i << ": " << argv[i] << endl;
        }
        
    } else {
        // Stampo un messaggio di errore se non è stato passato alcun argomento
        cout << "Errore: nessun argomento passato." << endl;
    }

    return 0;
}
```

Ora eseguendo questo programma e passando alcuni argomenti, ad esempio "programma.exe arg1 arg2 arg3", otterrai il seguente output:

```
Argomento 1: arg1
Argomento 2: arg2
Argomento 3: arg3
```

## Deep Dive

Se vuoi approfondire la lettura degli argomenti della riga di comando, puoi anche utilizzare la libreria <sstream> per convertire gli argomenti in altri tipi di dati, ad esempio interi o float. Inoltre, ricorda che il parametro **argv[0]** contiene il nome del programma, quindi il primo argomento utile sarà **argv[1]**.

Inoltre, è possibile utilizzare la funzione std::stoi() per convertire una stringa in un intero, o std::stof() per convertire una stringa in un float. Ad esempio:

```C++
#include <iostream>
#include <sstream> // Libreria per la conversione di tipi di dati
using namespace std;

int main(int argc, char* argv[]) {
    
    // Controllo se sono stati passati degli argomenti
    if(argc > 1) {
        // Converto il terzo argomento da stringa a intero e lo stampo
        int numero = std::stoi(argv[3]);
        cout << "Il terzo argomento è un intero: " << numero << endl;
        
        // Converto il quarto argomento da stringa a float e lo moltiplico per 2
        float numero2 = std::stof(argv[4]);
        float risultato = numero2 * 2;
        cout << "Il quarto argomento moltiplicato per 2 è: " << risultato << endl;
        
    } else {
        // Stampo un messaggio di errore se non è stato passato alcun argomento
        cout << "Errore: nessun argomento passato." << endl;
    }
    
    return 0;
}
```

## Vedi Anche

- [Come leggere la riga di comando in C++](https://www.programmareincpp.it/capitolo1/leggere-gli-argomenti-della-riga-di-comando-in-c/)
- [Convertire una stringa in un intero o float in C++](https://www.next.academy/it/blog/convertire-stringa-in-integer-float-c-plus-plus/)