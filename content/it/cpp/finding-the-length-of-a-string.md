---
title:    "C++: Trovare la lunghezza di una stringa"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è un'operazione comune quando si lavora con il linguaggio di programmazione C++. Sapere come farlo è essenziale per scrivere codice efficiente e funzionale. In questo articolo, scopriremo come trovare la lunghezza di una stringa in C++.

## Come fare

Per trovare la lunghezza di una stringa in C++, è necessario utilizzare la funzione integrata `strlen()`. Questa funzione ha bisogno di un argomento, che è la stringa che si desidera analizzare, e restituirà il numero di caratteri nella stringa.

Ecco un esempio di come utilizzare la funzione `strlen()` in C++:

```C++
#include <iostream>
#include <cstring>

int main() {
    char str[] = "Ciao amici!";
    int len = strlen(str);
    std::cout << "La lunghezza della stringa è: " << len << std::endl;
    return 0;
}
```

In questo esempio, stiamo utilizzando la libreria standard `cstring` che ci permette di utilizzare la funzione `strlen()`. Abbiamo definito una variabile `len` per memorizzare il risultato della funzione. Infine, stampiamo il risultato utilizzando `std::cout`.

L'output di questo codice sarà:

`La lunghezza della stringa è: 11`

## Approfondimento

Oltre alla funzione `strlen()`, esistono altre due funzioni integrare che possono essere utilizzate per trovare la lunghezza di una stringa in C++: `sizeof()` e il metodo `length()`.

La funzione `sizeof()` restituirà la dimensione totale della variabile che si sta analizzando, inclusi i caratteri di terminazione speciale. Quindi, per trovare la lunghezza della stringa, è necessario sottrarre 1 dal risultato.

Il metodo `length()`, invece, può essere utilizzato solo con gli oggetti stringa della libreria standard di C++. Questo metodo restituirà la lunghezza esatta della stringa, senza dover sottrarre 1.

Ecco un esempio di come utilizzare `sizeof()` e `length()` in C++:

```C++
#include <iostream>
#include <string>

int main() {
    char str[] = "Ciao amici!";
    int len1 = sizeof(str) - 1;
    std::cout << "La lunghezza della stringa utilizzando sizeof() è: " << len1 << std::endl;
    std::string str2 = "Ciao amici!";
    int len2 = str2.length();
    std::cout << "La lunghezza della stringa utilizzando length() è: " << len2 << std::endl;
    return 0;
}
```

L'output di questo codice sarà:

```
La lunghezza della stringa utilizzando sizeof() è: 11
La lunghezza della stringa utilizzando length() è: 11
```

## Vedi anche

- [Funzioni di manipolazione delle stringhe in C++](https://www.geeksforgeeks.org/string-manipulation-in-cc/)
- [Tutorial di C++ su strlen()](https://www.tutorialspoint.com/cplusplus-string-strlen)
- [Documentazione di C++ sulla libreria string](https://www.cplusplus.com/reference/string/)