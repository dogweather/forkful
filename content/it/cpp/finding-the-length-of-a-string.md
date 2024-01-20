---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Arduino: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Trovare la lunghezza di una stringa in C++

## Cos'è e perché?
Trovar la lunghezza di una stringa significa determinare il numero di caratteri che la compongono. Questo è un'operazione comune nella programmazione, da utilizzare quando, ad esempio, si devono confrontare stringhe o verificarne il contenuto.

## Come fare:
In C++, la classe `string` fornisce il metodo `length()` per calcolare la lunghezza di una stringa. Per esempio:

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Ciao, mondo!";
    std::cout << str.length();
    return 0;
}
```

Questo codice produrrebbe l'output `12`, che è la lunghezza della stringa "Ciao, mondo!" inclusi gli spazi e la punteggiatura.

## Approfondimento
Le stringhe non sono sempre state manipolabili così facilmente in C++. Nel linguaggio C, da cui deriva C++, una stringa era un array di caratteri e non aveva un metodo `length()`. Il programmatore doveva scrivere un ciclo per determinare la lunghezza della stringa.

Un'alternativa al metodo `length()` della classe `string` è la funzione `strlen()` della libreria `cstring`, che opera sugli array di caratteri:

```C++
#include <iostream>
#include <cstring>

int main() {
    char str[] = "Ciao, mondo!";
    std::cout << strlen(str);
    return 0;
}
```

Ma `strlen()` può creare problemi di sicurezza se usato incautamente, perché non verifica se l'array termina con un carattere null, rischiando quindi di causare un overflow del buffer.

## Per saperne di più
Per approfondire l'argomento, potete consultare i seguenti collegamenti.

- Documentazione ufficiale sulla classe `string`: http://www.cplusplus.com/reference/string/string/
- Guida alla funzione `strlen()`: http://www.cplusplus.com/reference/cstring/strlen/
- Storia delle stringhe in C e C++: https://en.wikipedia.org/wiki/String_(C%2B%2B)