---
title:                "Att arbeta med TOML"
aliases:
- /sv/cpp/working-with-toml.md
date:                  2024-01-26T04:19:54.662200-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/working-with-toml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
TOML (Toms Uppenbara, Minimala Språk) är ett data-serialiseringsformat som är lätt att läsa på grund av dess tydliga semantik. Programmerare använder TOML för konfigurationsfiler eftersom det erbjuder en balans mellan mänsklig läsbarhet och maskinell tolkningsbarhet.

## Hur man gör:
För att arbeta med TOML i C++ behöver du ett bibliotek som `toml++`. Här är en snabbstart:

```C++
#include <toml++/toml.h>
#include <iostream>
#include <fstream>

int main() {
    // Tolka TOML från en fil
    std::ifstream ifs("config.toml");
    auto config = toml::parse(ifs);

    // Åtkomst till ett värde
    std::string title = config["title"].value_or("Otitled");
    std::cout << "Titel: " << title << '\n';

    // Modifiera och spara TOML
    config["title"] = "Ny Titel";
    std::ofstream ofs("config.toml");
    ofs << config;
}
```

Exempel `config.toml`:
```toml
title = "Exempel"
```

Exempel på utdata:
```plaintext
Titel: Exempel
```

## Fördjupning
TOML skapades av Tom Preston-Werner år 2013 som ett alternativ till YAML och JSON. Det är utformat för att vara enkelt och explicit, huvudsakligen för konfigurationsfiler. Till skillnad från JSON fokuserar TOML på att vara otvetydigt, vilket betyder att det är deterministiskt i hur dokumentet tolkas.

Alternativ till TOML inkluderar YAML, som är mer tillåtande i vad som är tillåtet, dock ibland på bekostnad av förutsägbarhet. JSON, ett annat alternativ, är ganska strikt i struktur men inte lika användarvänligt för konfigurationer på grund av brist på kommentarer och dess klammerintensiva syntax.

När det gäller implementering är `toml++` ett C++17-bibliotek bara med headerfiler som är kompatibelt med den senaste TOML-specifikationen. Det ger ett DOM-liknande gränssnitt för att navigera och manipulera TOML-data, vilket gör det enkelt att integrera i projekt. Biblioteket tar hand om tolkningen, valideringen och genereringen av utdata, vilket gör att du kan få och ställa in TOML-data med hjälp av C++-typer.

## Se även
- TOML:s GitHub-repositorium: https://github.com/toml-lang/toml
- `toml++`, ett C++-bibliotek för TOML: https://github.com/marzer/tomlplusplus
- Den officiella TOML-dokumentationen med detaljerade förklaringar av formatet: https://toml.io/en/
