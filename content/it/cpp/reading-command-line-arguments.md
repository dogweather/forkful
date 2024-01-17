---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "C++: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Il lettore di argomenti della riga di comando è una funzionalità importante nella programmazione C++, che consente ai programmatori di passare informazioni da riga di comando al loro programma. Ciò è utile per creare un programma più flessibile e personalizzabile. 

## Come fare:
Ecco un esempio di codice C++ per leggere gli argomenti della riga di comando e stamparli: 

```C++
#include <iostream>
using namespace std;

int main(int argc, char *argv[])
{
    // loop attraverso tutti gli argomenti della riga di comando
    for (int i = 0; i < argc; i++) {
        // stampa l'indice dell'argomento e il suo valore
        cout << "Argomento #" << i << ": " << argv[i] << endl;
    }
} 
```

Esempio di output:
```
Argomento #0: ./esempio
Argomento #1: arg1
Argomento #2: arg2
```

## Approfondimento:
### Contesto storico:
La lettura degli argomenti della riga di comando è stata introdotta per la prima volta nel linguaggio C nel 1972. È diventata una parte integrante della programmazione moderna e continua ad essere utilizzata nel C++ e in altri linguaggi di programmazione.

### Alternative:
Alcune alternative alla lettura degli argomenti della riga di comando includono l'utilizzo di variabili globali o la lettura da file. Tuttavia, la lettura degli argomenti della riga di comando è spesso preferita poiché è più semplice e diretta.

### Dettagli di implementazione:
Quando il programma viene lanciato, il sistema operativo imposta una variabile, chiamata "argc", che contiene il numero di argomenti nella riga di comando. Viene anche creato un array di stringhe, chiamato "argv", che contiene gli argomenti stessi. Il programma può quindi utilizzare queste informazioni per leggere e utilizzare gli argomenti.

## Vedi anche:
Per ulteriori informazioni sulla lettura degli argomenti della riga di comando in C++, consulta la documentazione ufficiale di C++ o altre risorse online.