---
title:                "Rimuovere le virgolette da una stringa"
aliases:
- /it/cpp/removing-quotes-from-a-string/
date:                  2024-01-26T03:37:54.795009-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rimuovere le virgolette da una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Rimuovere le virgolette da una stringa significa liberarsi di quei fastidiosi caratteri doppi o singoli che racchiudono il nostro testo (' o "). I programmatori spesso fanno ciò per sanificare l'input, memorizzare testo in un database o preparare le stringhe per ulteriori elaborazioni senza l'ingombro delle virgolette.

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
