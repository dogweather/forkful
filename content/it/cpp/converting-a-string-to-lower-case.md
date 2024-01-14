---
title:                "C++: Convertire una stringa in minuscolo"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Il processo di conversione di una stringa in minuscolo è un'operazione fondamentale nella programmazione C++. Questa funzione permette di standardizzare le stringhe inserite dall'utente, facilitando così la gestione dei dati all'interno del programma.

## Come fare
La conversione di una stringa in minuscolo è possibile grazie alla funzione "tolower()" che appartiene alla libreria standard di C++. Questa funzione si applica a singoli caratteri di una stringa, convertendoli uno alla volta.

```C++
// Esempio di codice per convertire una stringa in minuscolo
#include <iostream>
#include <cstring>
using namespace std;

int main() {
    char stringa[20];

    // Inserimento della stringa da parte dell'utente
    cout << "Inserisci una stringa: ";
    cin >> stringa;

    // Ciclo for per convertire ogni carattere in minuscolo
    for (int i = 0; i < strlen(stringa); i++) {
        stringa[i] = tolower(stringa[i]);
    }

    // Output della stringa convertita
    cout << "La stringa convertita in minuscolo è: " << stringa << endl;

    return 0;
}
```
Output:
```
Inserisci una stringa: Hello World
La stringa convertita in minuscolo è: hello world
```

## Approfondimento
La funzione "tolower()" utilizza la codifica ASCII dei caratteri per convertirli in minuscolo. In questa codifica, i caratteri alfabetici maiuscoli sono numerati da 65 a 90, mentre quelli minuscoli sono numerati da 97 a 122. Utilizzando questa differenza di 32, la funzione "tolower()" converte i caratteri maiuscoli in minuscoli sottraendo 32 dalla loro codifica ASCII. Pertanto, è importante assicurarsi che i caratteri siano nella gamma corretta prima di applicare la funzione "tolower()" per evitare risultati imprevisti.

## Vedi anche
- [Funzione "tolower()" nella documentazione di C++](https://www.cplusplus.com/reference/cctype/tolower/)
- [Codifica ASCII su Wikipedia](https://it.wikipedia.org/wiki/ISO/IEC_8859-1)