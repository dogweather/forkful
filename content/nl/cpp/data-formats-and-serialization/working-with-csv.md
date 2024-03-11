---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:13.013916-07:00
description: "Werken met CSV (Comma-Separated Values) betekent het omgaan met platte\
  \ tekstbestanden die tabelgegevens opslaan. Programmeurs gebruiken CSV omdat het\u2026"
lastmod: '2024-03-11T00:14:24.973857-06:00'
model: gpt-4-0125-preview
summary: "Werken met CSV (Comma-Separated Values) betekent het omgaan met platte tekstbestanden\
  \ die tabelgegevens opslaan. Programmeurs gebruiken CSV omdat het\u2026"
title: Werken met CSV
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met CSV (Comma-Separated Values) betekent het omgaan met platte tekstbestanden die tabelgegevens opslaan. Programmeurs gebruiken CSV omdat het eenvoudig is en compatibel over verschillende systemen, perfect voor het uitwisselen van gegevens tussen verschillende software.

## Hoe te:

Hier is een stuk code dat een CSV-bestand leest en de inhoud ervan afdrukt.

```C++
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>

int main() {
    std::string regel, cel;
    std::vector<std::vector<std::string>> csvGegevens;
    std::ifstream bestand("voorbeeld.csv");

    while (std::getline(bestand, regel)) {
        std::stringstream regelStroom(regel);
        std::vector<std::string> rijGegevens;
        
        while (std::getline(regelStroom, cel, ',')) {
            rijGegevens.push_back(cel);
        }
        csvGegevens.push_back(rijGegevens);
    }
    
    for (const auto& rij : csvGegevens) {
        for (const auto& kol : rij) {
            std::cout << kol << " ";  // Afhankelijk van je CSV-structuur, pas de scheidingslijn aan.
        }
        std::cout << std::endl;
    }
    return 0;
}
```

Voorbeelduitvoer voor een CSV die namen en leeftijden bevat:
```
John 25
Jane 28
```

## Diepgaand

CSV bestaat sinds het begin van de jaren 70. Het is het standaardformaat voor eenvoudige data-export en -import, maar is niet geweldig voor complexe hiërarchische gegevens, die door XML en JSON beter worden afgehandeld. C++ heeft geen ingebouwde ondersteuning voor CSV, maar het omgaan met bestanden en strings is eenvoudig. Je houdt je bezig met standaard I/O en stringmanipulatie, terwijl je uitkijkt voor speciale gevallen zoals aanhalingstekens en komma's binnen cellen. Bibliotheken zoals `libcsv` en `Boost.Tokenizer` kunnen taken vereenvoudigen als je met meer complexe CSV-bestanden te maken hebt.

## Zie Ook

- [RFC 4180](https://tools.ietf.org/html/rfc4180), het gemeenschappelijke formaat en MIME-type voor CSV-bestanden.
- [C++ referentie voor I/O](http://www.cplusplus.com/reference/fstream/)
- [De Boost C++ Bibliotheken](https://www.boost.org/)
- [10 minuten tot pandas - CSV-afhandeling met Python ter vergelijking](https://pandas.pydata.org/pandas-docs/stable/user_guide/10min.html)
