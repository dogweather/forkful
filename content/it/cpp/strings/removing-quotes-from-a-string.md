---
date: 2024-01-26 03:37:54.795009-07:00
description: 'Come fare: Ecco un modo diretto per eliminare quelle virgolette in C++.'
lastmod: '2024-03-13T22:44:43.714410-06:00'
model: gpt-4-0125-preview
summary: Ecco un modo diretto per eliminare quelle virgolette in C++.
title: Rimuovere le virgolette da una stringa
weight: 9
---

## Come fare:
Ecco un modo diretto per eliminare quelle virgolette in C++:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Ciao, 'Mondo'!")";
    std::string no_quotes = remove_quotes(original);
    std::cout << no_quotes << std::endl;
    return 0;
}
```

Esegui questo, e otterrai:

```
Ciao, Mondo!
```

Ecco fatto! Le virgolette sono sparite.

## Approfondimento
Le virgolette sono state una noia nel testo fin dall'alba della computazione. Nei vecchi tempi, vedevi i programmatori passare laboriosamente attraverso ogni carattere per filtrare quelle virgolette. Oggi, abbiamo `std::remove` nella Standard Template Library (STL) per fare il lavoro pesante.

Alternative? Certo! Potresti usare le espressioni regolari con `std::regex` per prendere di mira le virgolette, ma è un po' come usare un martello pneumatico per rompere una noce - potente, ma può essere eccessivo per compiti semplici. Per coloro che favoriscono le versioni recenti di C++, potreste cimentarvi con `std::string_view` per approcci non modificanti.

Per quanto riguarda l'implementazione, ricorda che `std::remove` non rimuove effettivamente gli elementi dal contenitore; sposta in avanti gli elementi non rimossi e restituisce un iteratore oltre la nuova fine dell'intervallo. Ecco perché abbiamo bisogno del metodo `erase` per tagliare via la coda indesiderata.

## Vedi Anche
- Riferimento per C++ `std::remove`: [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- Ulteriori informazioni sulla manipolazione di `std::string`: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
