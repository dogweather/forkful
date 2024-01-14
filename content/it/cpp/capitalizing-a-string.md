---
title:                "C++: Capitalizzare una stringa"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché 

Capitalize una stringa è un'operazione comune nel programma di sviluppo. Ciò può essere utile quando si vuole dare enfasi a un titolo o a una parola importante. Inoltre, può essere necessario per avere uniformità nei dati, ad esempio quando si lavora con elenchi di nomi.

## Come fare 

Per capitalizzare una stringa in C++, si può utilizzare la funzione `toupper()` che converte tutti i caratteri della stringa in maiuscolo. Oppure, si può utilizzare un semplice ciclo `for` per scorrere la stringa e convertire i caratteri desiderati in maiuscolo. Di seguito sono riportati due esempi di codice e relativi output:

```C++
// Utilizzando la funzione toupper()
#include <iostream>
#include <string>
using namespace std;

int main() {
    string parola = "esempio";
    transform(parola.begin(), parola.end(), parola.begin(), ::toupper);
    cout << parola << endl;
    return 0;
}
// Output: ESEMPIO
```

```C++
// Utilizzando il ciclo for
#include <iostream>
#include <string>
using namespace std;

int main() {
    string parola = "esempio";
    for (int i = 0; i < parola.length(); i++) {
        parola[i] = toupper(parola[i]);
    }
    cout << parola << endl;
    return 0;
}
// Output: ESEMPIO
```

## Approfondimento 

Oltre alla funzione `toupper()`, esiste anche la funzione `tolower()` che converte tutti i caratteri di una stringa in minuscolo. Inoltre, è possibile utilizzare la libreria `<cctype>` che contiene vari metodi per manipolare i caratteri, come ad esempio `isupper()` per verificare se un carattere è maiuscolo e `islower()` per verificare se un carattere è minuscolo.

## Vedi anche 

- [Documentazione di toupper()](https://www.cplusplus.com/reference/cctype/toupper/)
- [Tutorial su come manipolare le stringhe in C++](https://www.studytonight.com/cpp/string-manipulation-functions-in-cpp)
- [Guida completa a C++](https://www.geeksforgeeks.org/c-plus-plus/)
- [C++ per principianti](https://www.sololearn.com/Play/CPlusPlus/)