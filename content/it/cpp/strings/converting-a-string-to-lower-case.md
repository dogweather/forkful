---
aliases:
- /it/cpp/converting-a-string-to-lower-case/
date: 2024-01-20 17:38:00.222280-07:00
description: "Convertire una stringa in minuscolo significa trasformare tutti i caratteri\
  \ alfabetici maiuscoli in minuscoli. Questo \xE8 utile per uniformare i dati,\u2026"
lastmod: 2024-02-18 23:08:56.160721
model: gpt-4-1106-preview
summary: "Convertire una stringa in minuscolo significa trasformare tutti i caratteri\
  \ alfabetici maiuscoli in minuscoli. Questo \xE8 utile per uniformare i dati,\u2026"
title: Conversione di una stringa in minuscolo
---

{{< edit_this_page >}}

## What & Why?
Convertire una stringa in minuscolo significa trasformare tutti i caratteri alfabetici maiuscoli in minuscoli. Questo è utile per uniformare i dati, facilitare confronti e ricerche nel testo, indipendentemente dalla capitalizzazione.

## How to:
Esempio: usando la funzione `std::transform` e `::tolower`.

```C++
#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>

int main() {
    std::string s = "Ciao Mondo!";
    std::transform(s.begin(), s.end(), s.begin(),
                   [](unsigned char c){ return std::tolower(c); });
    std::cout << s << std::endl;
    return 0;
}
```

Output:
```
ciao mondo!
```

## Deep Dive
La conversione delle stringhe in minuscolo risale ai primi giorni dell'informatica, quando i sistemi erano meno tolleranti alle varianti di maiuscole e minuscole. In C++, prima dell'introduzione della libreria standard, i programmatori spesso scrivevano le proprie funzioni per manipolare le stringhe.

Alternativamente, possiamo usare delle funzioni di una libreria esterna come Boost, che presenta funzioni come `to_lower_copy` per creare nuove stringhe in minuscolo, oppure la manipolazione diretta con cicli e funzioni `tolower`.

Dettaglio implementativo: `std::tolower` necessita di `unsigned char` come input per evitare comportamenti indefiniti con caratteri la cui rappresentazione supera il valore di `CHAR_MAX`.

## See Also
- [cppreference.com: std::tolower](https://en.cppreference.com/w/cpp/string/byte/tolower)
- [cppreference.com: std::transform](https://en.cppreference.com/w/cpp/algorithm/transform)
