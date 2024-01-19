---
title:                "Mettere in Maiuscolo una Stringa"
html_title:           "C++: Mettere in Maiuscolo una Stringa"
simple_title:         "Mettere in Maiuscolo una Stringa"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

La capitalizzazione di una stringa implica la trasformazione di tutti i suoi caratteri in maiuscolo. I programmatori lo fanno per diverse ragioni, ad esempio per migliorare la leggibilità o per normalizzare i dati input.

## Come fare: 

Ecco un esempio di come capitalizzare una stringa in C++ utilizzando la libreria `algorithm` e la funzione `::toupper`.

```C++
#include <algorithm>
#include <cctype>
#include <iostream>
#include <string> 

int main() {
    std::string s = "questa è una stringa";
    std::transform(s.begin(), s.end(), s.begin(),
                   [](unsigned char c) { return std::toupper(c); });
 
    std::cout << s; 
}
```

Output:

```
QUESTA È UNA STRINGA
```

## Approfondimento 

La funzione `toupper` esiste già da molto tempo nelle librerie standard del C, prima ancora dell'arrivo del C++. 

Un'alternativa è l'uso del locale con `std::toupper`, che rispetta le caratteristiche della lingua locale.

Dettaglio implementativo: `std::transform` modifica direttamente la stringa originale, risparmiando memoria.

## Vedere Anche 

- [Funzione std::toupper](http://www.cplusplus.com/reference/cctype/toupper/)
- [Funzione std::transform](http://www.cplusplus.com/reference/algorithm/transform/)
- [Standard Library cctype](http://www.cplusplus.com/reference/cctype/)
- [Libreria Standard di algoritmi](http://www.cplusplus.com/reference/algorithm/)