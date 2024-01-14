---
title:                "C++: Trovare la lunghezza di una stringa"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Perchè

Trovare la lunghezza di una stringa è un'operazione fondamentale nella programmazione di C++. Sapere quanti caratteri compongono una stringa ci permette di gestirle e manipolarle in modo preciso e efficiente.

## Come fare

Per trovare la lunghezza di una stringa in C++, esistono varie opzioni. La più semplice è utilizzare la funzione integrata `strlen()`, presente nella libreria `<string.h>`. Questa funzione prende come argomento una stringa e restituisce il numero di caratteri che la compongono. Ecco un esempio di codice:

```C++
#include <iostream>
#include <string.h>

int main() {
    char str[] = "Ciao a tutti!";
    int lunghezza = strlen(str);
    std::cout << "La stringa \"" << str << "\" ha " << lunghezza << " caratteri." << std::endl;
    return 0;
}
```

Output:

```
La stringa "Ciao a tutti!" ha 13 caratteri.
```

Un'altra opzione è utilizzare il metodo `length()` dell'oggetto stringa della libreria standard (`<string>`). Questo metodo restituisce un valore di tipo `size_t`, che corrisponde a un intero non negativo. Ecco un esempio di codice:

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Ciao a tutti!";
    size_t lunghezza = str.length();
    std::cout << "La stringa \"" << str << "\" ha " << lunghezza << " caratteri." << std::endl;
    return 0;
}
```

Output:

```
La stringa "Ciao a tutti!" ha 13 caratteri.
```

## Approfondimenti

Esistono alcune differenze tra le due opzioni presentate per trovare la lunghezza di una stringa. Ad esempio, la funzione `strlen()` conta solo i caratteri fino al primo carattere di terminazione della stringa (`'\0'`), mentre il metodo `length()` controlla tutto il contenuto della stringa, incluso il carattere di terminazione.

Inoltre, la funzione `strlen()` richiede come argomento un array di caratteri (`char[]`) mentre il metodo `length()` richiede un oggetto stringa (`string`). Ciò significa che dobbiamo essere attenti a quale tipo di dato utilizziamo per evitare potenziali errori.

## Vedi anche

- [Funzione `strlen()` in C++](https://www.cplusplus.com/reference/cstring/strlen/)
- [Metodo `length()` in C++](https://www.cplusplus.com/reference/string/string/length/)
- [Stringhe in C++](https://www.cplusplus.com/doc/tutorial/ntcs/)