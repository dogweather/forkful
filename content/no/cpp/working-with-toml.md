---
title:                "Jobbe med TOML"
date:                  2024-01-26T04:19:52.578259-07:00
model:                 gpt-4-0125-preview
simple_title:         "Jobbe med TOML"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/working-with-toml.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
TOML (Toms Åpenbare, Minimale Språk) er et data serialiseringsformat som er lett å lese på grunn av sin klare semantikk. Programmerere bruker TOML for konfigurasjonsfiler fordi det gir en balanse mellom menneskelig lesbarhet og maskinens tolkbarhet.

## Hvordan:
For å arbeide med TOML i C++, trenger du et bibliotek som `toml++`. Her er en rask start:

```C++
#include <toml++/toml.h>
#include <iostream>
#include <fstream>

int main() {
    // Parse TOML fra en fil
    std::ifstream ifs("config.toml");
    auto config = toml::parse(ifs);

    // Tilgang til en verdi
    std::string title = config["title"].value_or("Uten tittel");
    std::cout << "Tittel: " << title << '\n';

    // Endre og lagre TOML
    config["title"] = "Ny Tittel";
    std::ofstream ofs("config.toml");
    ofs << config;
}
```

Eksempel på `config.toml`:
```toml
title = "Eksempel"
```

Eksempel på output:
```plaintext
Tittel: Eksempel
```

## Dypdykk
TOML ble skapt av Tom Preston-Werner i 2013 som et alternativ til YAML og JSON. Det er designet for å være enkelt og eksplisitt, hovedsakelig for konfigurasjonsfiler. I motsetning til JSON fokuserer TOML på å være utvetydig, noe som betyr at det er deterministisk i hvordan dokumentet blir tolket.

Alternativer til TOML inkluderer YAML, som er mer tillatende i hva som er tillatt, skjønt noen ganger på bekostning av forutsigbarhet. JSON, et annet alternativ, er ganske streng i struktur, men ikke så menneskevennlig for konfigurasjoner på grunn av mangel på kommentarer og dets klammesvære syntaks.

I implementering, er `toml++` et header-only C++17 bibliotek som er i samsvar med den nyeste TOML-spesifikasjonen. Det tilbyr et DOM-lignende grensesnitt for å navigere og manipulere TOML-data, noe som gjør det greit å integrere i prosjekter. Biblioteket tar seg av parsing, validering og generering av output, slik at du kan få og sette TOML-data ved hjelp av C++-typer.

## Se også
- TOML GitHub-repositoriet: https://github.com/toml-lang/toml
- `toml++`, et C++ bibliotek for TOML: https://github.com/marzer/tomlplusplus
- Den offisielle TOML-dokumentasjonen med detaljerte forklaringer på formatet: https://toml.io/en/