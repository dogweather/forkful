---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

La conversione di una stringa in minuscolo è un'operazione comune nella programmazione C++, che trasforma tutte le lettere maiuscole in una stringa in minuscolo. I programmatori lo fanno per motivi di confronto e per garantire l'omogeneità dei dati.

## Come fare:

Ecco un esempio semplice su come convertire una stringa in minuscolo usando la libreria standard C++.

```C++
#include <cctype>
#include <iostream>
#include <algorithm>

int main()
{
    std::string str = "CIAO, MONDO!";
    std::transform(str.begin(), str.end(), str.begin(),
        [](unsigned char c){ return std::tolower(c); });

    std::cout << str << std::endl;
    return 0;
}
```

In output si vedrà:

```
ciao, mondo!
```

## Un tuffo nel profondo

Historicamente, la conversione di stringhe è stata un'operazione essenziale in programmazione sin dai suoi primi giorni. In C++, abbiamo diversi modi per convertire una stringa in minuscolo. Oltre alla funzione `std::transform`, possiamo utilizzare cicli for per farlo manualmente.

```C++
std::string str = "CIAO, MONDO!";
for(auto & c: str) c = std::tolower(c);
```

Riguardo ai dettagli di implementazione, ricorda che le funzioni come `std::tolower` operano sulla base dei valori ASCII dei caratteri. Le lettere maiuscole hanno valori ASCII da 65 a 90 e le minuscole da 97 a 122. Quando chiamiamo `std::tolower`, aggiunge essenzialmente 32 al valore ASCII del carattere, convertendolo in rispettiva versione minuscola.

## Vedi anche:

Per ulteriori informazioni sulla lavorazione delle stringhe in C++, si possono consultare le seguenti risorse:

- [C++ Standard Library: String](http://www.cplusplus.com/reference/string/string/)
- [C++ Algorithm Library](http://en.cppreference.com/w/cpp/algorithm)
- [C++ ctype library](http://www.cplusplus.com/reference/cctype/)