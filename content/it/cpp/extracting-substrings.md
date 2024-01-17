---
title:                "Estrazione di sottostringhe"
html_title:           "C++: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Che cos'è e perché:
Estrarre sottostringhe è il processo di selezione di una parte di una stringa più grande. I programmatori spesso lo fanno per analizzare e manipolare dati in modo più preciso e efficiente.

## Come fare:
Ecco un esempio di codice in C++ che mostra come estrarre una sottostringa da una stringa più grande utilizzando la funzione `substr()`:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "Ciao mondo!";
    string subStr = str.substr(5, 5); // estrae "mondo" dalla posizione 5 con una lunghezza di 5 caratteri
    cout << subStr; // output: mondo
    return 0;
}
```

## Approfondimento:
Estrarre sottostringhe è un concetto comune in programmazione. In C++ ci sono diverse alternative come ad esempio l'utilizzo delle funzioni `strncpy()` e `memcpy()`. La funzione `substr()` è stata introdotta nella libreria standard di C++ a partire dalla versione 98 e offre una sintassi più semplice e intuitiva.

## Vedi anche:
- [La documentazione ufficiale di C++ sulla funzione `substr()`](https://en.cppreference.com/w/cpp/string/basic_string/substr)
- [Un tutorial su come utilizzare la funzione `substr()` in C++](https://www.studytonight.com/cpp/string-basic-string-substr-function.php)
- [Altre alternative per estrarre sottostringhe in C++](https://www.cplusplus.com/forum/general/78848/)