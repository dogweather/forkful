---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:04.024540-07:00
description: "Het parsen van een datum uit een tekenreeks betekent het omzetten van\
  \ tekst naar een datumgegevenstype. Programmeurs doen dit om datumgerelateerde logica\u2026"
lastmod: '2024-02-25T18:49:48.455561-07:00'
model: gpt-4-0125-preview
summary: "Het parsen van een datum uit een tekenreeks betekent het omzetten van tekst\
  \ naar een datumgegevenstype. Programmeurs doen dit om datumgerelateerde logica\u2026"
title: Een datum uit een string parsen
---

{{< edit_this_page >}}

## Wat & Waarom?
Het parsen van een datum uit een tekenreeks betekent het omzetten van tekst naar een datumgegevenstype. Programmeurs doen dit om datumgerelateerde logica op een gestandaardiseerde, locatie-onafhankelijke manier te behandelen, vaak voor taken zoals invoervalidatie, sortering en opslag.

## Hoe:
Gebruik `<chrono>` en `<sstream>` om een datum in C++ te parsen. Hier is een snel voorbeeld:

```C++
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string datum_tekst = "2023-04-01";
    std::istringstream ss(datum_tekst);
    std::chrono::year_month_day geparseerde_datum;
    
    ss >> std::chrono::parse("%F", geparseerde_datum);
    if (ss.fail()) {
        std::cout << "Parsen mislukt\n";
        return 1;
    }

    std::cout << "Jaar: " << int(geparseerde_datum.year()) << '\n';
    std::cout << "Maand: " << unsigned(geparseerde_datum.month()) << '\n';
    std::cout << "Dag: " << unsigned(geparseerde_datum.day()) << '\n';

    return 0;
}
```

Voorbeelduitvoer:
```
Jaar: 2023
Maand: 4
Dag: 1
```

## Diepgaande Duik
Het parsen van datums uit tekenreeksen is niet nieuw. In de dagen van C was `strptime` typerend. In modern C++ is `<chrono>` je vriend. Het scheidt netjes de zorgen: formatteren/parsen met `std::chrono::parse`, en datummanipulatie met `std::chrono` types.

Voor C++20, zou je waarschijnlijk voor `std::get_time` of third-party bibliotheken zoals Boost kiezen. Na C++20 kreeg de standaardbibliotheek een glanzende upgrade met verbeteringen aan `std::chrono`. Nu krijg je type-veilige datumtypen en functies out-of-the-box.

De parseerfunctie, `std::chrono::parse`, is veelzijdig, en begrijpt vele datum- en tijdformaten. Het "%F" formaat dat we hierboven gebruiken, is het ISO 8601 datumformaat (jaar-maand-dag). Maar je kunt ook met andere formaten omgaan, pas gewoon de formaatreeks dienovereenkomstig aan.

Onthoud, ondanks robuust parsen, is gebruikersinvoer lastig. Behandel parseerfouten altijd gracieus, zoals gedaan met `ss.fail()` in het voorbeeld.

## Zie Ook
Duik dieper in `<chrono>` met de officiële [cppreference pagina](https://en.cppreference.com/w/cpp/header/chrono).

Krijg historische context van Stroustrup's kijk op de geschiedenis van C++ op [The Design and Evolution of C++](http://www.stroustrup.com/hopl2.pdf).

Voor randgevallen of niet-standaard formaten, overweeg om [Boost.DateTime](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html) te bekijken.
