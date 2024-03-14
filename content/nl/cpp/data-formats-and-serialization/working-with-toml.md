---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:44.409957-07:00
description: "TOML (Tom's Obvious, Minimal Language) is een gegevensserialisatieformaat\
  \ dat makkelijk te lezen is vanwege de duidelijke semantiek. Programmeurs\u2026"
lastmod: '2024-03-13T22:44:51.136440-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) is een gegevensserialisatieformaat\
  \ dat makkelijk te lezen is vanwege de duidelijke semantiek. Programmeurs\u2026"
title: Werken met TOML
---

{{< edit_this_page >}}

## Wat & Waarom?
TOML (Tom's Obvious, Minimal Language) is een gegevensserialisatieformaat dat makkelijk te lezen is vanwege de duidelijke semantiek. Programmeurs gebruiken TOML voor configuratiebestanden omdat het een balans biedt tussen leesbaarheid voor mensen en analyseerbaarheid door machines.

## Hoe te gebruiken:
Om met TOML in C++ te werken, heb je een bibliotheek zoals `toml++` nodig. Hier is een snelle start:

```C++
#include <toml++/toml.h>
#include <iostream>
#include <fstream>

int main() {
    // TOML uit een bestand parseren
    std::ifstream ifs("config.toml");
    auto config = toml::parse(ifs);

    // Een waarde toegankelijk maken
    std::string title = config["title"].value_or("Zonder titel");
    std::cout << "Titel: " << title << '\n';

    // Wijzig en sla TOML op
    config["title"] = "Nieuwe Titel";
    std::ofstream ofs("config.toml");
    ofs << config;
}
```

Voorbeeld `config.toml`:
```toml
title = "Voorbeeld"
```

Voorbeeld van uitvoer:
```plaintext
Titel: Voorbeeld
```

## Diepe Duik
TOML is in 2013 gecreëerd door Tom Preston-Werner als een alternatief voor YAML en JSON. Het is ontworpen om eenvoudig en expliciet te zijn, voornamelijk voor configuratiebestanden. In tegenstelling tot JSON, focust TOML op het zijn van ondubbelzinnig, wat betekent dat het deterministisch is in hoe het document wordt geparst.

Alternatieven voor TOML omvatten YAML, dat meer toelaat in wat er mogelijk is, hoewel soms ten koste van de voorspelbaarheid. JSON, een ander alternatief, is vrij strikt in structuur maar niet zo gebruiksvriendelijk voor configuraties vanwege het gebrek aan commentaar en de veelheid aan accolades.

In implementatie is `toml++` een header-only C++17 bibliotheek die overeenkomt met de laatste TOML specificatie. Het biedt een DOM-achtige interface om door TOML-gegevens te navigeren en deze te manipuleren, waardoor het eenvoudig te integreren is in projecten. De bibliotheek zorgt voor het parsen, valideren en genereren van uitvoer, waardoor je TOML-gegevens kunt krijgen en instellen met C++-typen.

## Zie Ook
- De TOML GitHub-repository: https://github.com/toml-lang/toml
- `toml++`, een C++ bibliotheek voor TOML: https://github.com/marzer/tomlplusplus
- De officiële TOML-documentatie met gedetailleerde uitleg van het formaat: https://toml.io/nl/
