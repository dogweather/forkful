---
title:                "Lettura di un file di testo"
html_title:           "C++: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore C++ e stai lavorando con file di testo, è importante sapere come leggere questi file nel tuo programma. In questo articolo, scoprirai come farlo in modo efficiente utilizzando il linguaggio C++.

## Come fare

Per leggere un file di testo in C++, devi seguire questi semplici passaggi:

1. Includere la libreria \<fstream\> nel tuo programma.
2. Dichiarare un oggetto di tipo `ifstream` e aprirlo utilizzando il nome del file come parametro.
3. Utilizza il metodo `getline()` per leggere una riga dal file e salvarla in una variabile di tipo `string`.
4. Ripeti questo passaggio fino a quando non hai letto tutte le righe del file.

Ecco un esempio di codice che legge un file di testo e stampa il suo contenuto su schermo:

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
    ifstream file("esempio.txt");
    string line;
    
    while (getline(file, line)) {
        cout << line << endl;
    }
    
    file.close();
    
    return 0;
}
```

L'output di questo programma sarà:

```
Questo è un esempio
di un file di testo
utilizzato per questo articolo.
```

## Approfondimento

Oltre alla lettura delle righe del file di testo, è possibile utilizzare altri metodi per leggere e manipolare il suo contenuto. Ad esempio, puoi utilizzare il metodo `get()` per leggere un singolo carattere alla volta o il metodo `ignore()` per saltare una certa quantità di caratteri.

Inoltre, se devi leggere un file binario, devi dichiarare l'oggetto `ifstream` come `ifstream::binary` e utilizzare il metodo `read()` per leggere un numero specifico di byte.

## Vedi anche

- [C++ - ifstream](https://www.cplusplus.com/reference/fstream/ifstream/)
- [Lettura di un file di testo in C++](https://www.geeksforgeeks.org/reading-text-file-c/)
- [C++ - Manipolazione di file](https://www.programiz.com/cpp-programming/files-input-output)