---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
html_title:           "Bash: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizzare una stringa significa trasformare tutti i suoi caratteri in lettere maiuscole. I programmatori lo fanno per uniformità, enfasizzare qualcosa, o preparare i dati per confronti che non tengono conto della differenza tra maiuscole e minuscole.

## How to:
```C++
#include <iostream>
#include <algorithm>
#include <cctype>
#include <string>

int main() {
    std::string testo = "Ciao, come va?";
    std::transform(testo.begin(), testo.end(),testo.begin(), 
        [](unsigned char c){ return std::toupper(c); });

    std::cout << testo << std::endl; // Output: CIAO, COME VA?
    return 0;
}
```

## Deep Dive
Capitalizzare stringhe è una pratica comune da decenni. In C++, funzioni come `std::toupper` sono parte della libreria standard e fanno esattamente questo. Alternativamente, puoi scrivere il tuo ciclo per passare attraverso la stringa e usare `std::toupper` su ogni carattere. Prima di C++11, i programmatori usavano spesso cicli manuali, ma l’introduzione delle lambda functions e l'ampliamento delle funzionalità di `<algorithm>` hanno reso il codice più espressivo e compatto.

La funzione `std::toupper` funziona con `unsigned char` per evitare problemi di overflow con i caratteri firmati, quindi, quando usi questa funzione, è consigliabile effettuare un cast esplicito.

Inoltre, in contesti non C++, come i database o le interfacce utente, capitalizzare potrebbe aiutare a garantire coerenza indipendentemente dall'input degli utenti.

## See Also
- Documentazione di `std::transform`: https://en.cppreference.com/w/cpp/algorithm/transform
- Documentazione di `std::toupper`: https://en.cppreference.com/w/cpp/string/byte/toupper
- Articolo di CPP Reference su lambda functions: https://en.cppreference.com/w/cpp/language/lambda
