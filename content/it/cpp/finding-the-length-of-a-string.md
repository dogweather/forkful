---
title:    "C++: Trovare la lunghezza di una stringa"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza delle stringhe è un'operazione comune in programmazione e può essere utile in molte situazioni diverse, come nel sito degli algoritmi o nella gestione delle stringhe utente in un programma.

## Come fare

Per trovare la lunghezza di una stringa in C++, è possibile utilizzare la funzione `length()` o `size()`, entrambe disponibili nella libreria standard `<string>`. Ecco un esempio di come utilizzare la funzione `length()`:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string nome = "Maria";
    int lunghezza = nome.length();
    cout << "La lunghezza della stringa 'nome' è: " << lunghezza << endl;
}
```

Output:
```
La lunghezza della stringa 'nome' è: 5
```

## Approfondimento

Quando si utilizza la funzione `length()` per trovare la lunghezza di una stringa, è importante tenere presente che questa conta anche il carattere terminatore `\0`. Inoltre, se si utilizzano caratteri non ASCII nella stringa, la lunghezza potrebbe non corrispondere al numero di lettere effettive.

Per trovare la lunghezza di una stringa senza contare il terminatore `\0`, è possibile utilizzare il metodo `size()`. Inoltre, se si desidera controllare se una stringa è vuota, è possibile utilizzare il metodo `empty()`, che restituirà `true` se la stringa è vuota e `false` altrimenti.

Un'ulteriore nota importante è che i metodi `length()` e `size()` sono equivalenti e possono essere utilizzati in modo intercambiabile.

## Vedi anche
- [Documentazione C++: funzione `length()`](https://en.cppreference.com/w/cpp/string/basic_string/length)
- [Documentazione C++: funzione `size()`](https://en.cppreference.com/w/cpp/string/basic_string/size)
- [Documentazione C++: metodo `empty()`](https://en.cppreference.com/w/cpp/string/basic_string/empty)