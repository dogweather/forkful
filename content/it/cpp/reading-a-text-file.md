---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Si tratta di leggere dati da un file di testo nel tuo programma in C++. Questo è spesso fatto per gestire grandi quantità di dati che sarebbe impraticabile codificare direttamente nel nostro programma.

## Come fare:

Per leggere un file di testo in C++, possiamo usare la libreria standards `<fstream>`. Ecco un semplice esempio:

```C++
#include <fstream>
#include <iostream>
#include <string>

int main() {
    std::ifstream file("test.txt");
    std::string str; 

    while (std::getline(file, str)) {
        std::cout << str << std::endl; 
    }
    return 0;
}
```

In questo esempio, `"test.txt"` è il nome del file che vogliamo leggere. Il ciclo `while` continua a leggere linee dal file finché ci sono linee da leggere. Ogni linea viene stampata sulla console.

## Approfondimento:

In termini storici, la lettura di file di testo è stata una delle prime operazioni di I/O implementate nei linguaggi di programmazione, data la sua importanza per l'elaborazione di dati.

Come alternativa alla libreria `<fstream>`, ci sono altre librerie come `<cstdio>` che fornisce funzioni come `fopen` e `fscanf` per leggere file di testo. Tuttavia, `<fstream>` è più facile da usare e più sicuro in termini di gestione delle eccezioni.

I dettagli di implementazione nel leggere un file di testo variano in base al sistema operativo. La libreria `<fstream>` ne nasconde i dettagli e fornisce un'interfaccia semplificata per i programmatori.

## Vedere Anche:

1. Documentazione di `<fstream>`: http://www.cplusplus.com/reference/fstream/
2. Guida avanzata alla lettura di file di testo in C++: https://www.geeksforgeeks.org/reading-writing-text-files-c++
3. Libreria `<cstdio>`: http://www.cplusplus.com/reference/cstdio/.