---
title:                "Werken met TOML"
date:                  2024-01-28T22:10:59.172182-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met TOML"

category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
TOML is een gegevensserialisatietaal ontworpen om gemakkelijk te lezen en te schrijven. Programmeurs gebruiken het voor configuratiebestanden, eenvoudige gegevensopslag en kruis-taalgegevensuitwisseling vanwege de duidelijkheid en het gebruiksgemak voor mensen.

## Hoe te:
Laten we een TOML-configuratiebestand in C parseren met de "tomlc99" bibliotheek. Installeer eerst de bibliotheek. Maak dan een `config.toml`:

```toml
titel = "TOML Voorbeeld"

[eigenaar]
naam = "Tom Preston-Werner"
geb = 1979-05-27T07:32:00Z
```

Parseer het nu in C:

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("Fout: kan configuratiebestand niet openen\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("Fout: %s\n", errbuf);
        return 1;
    }

    printf("Titel: %s\n", toml_raw_in(conf, "titel"));

    toml_table_t* eigenaar = toml_table_in(conf, "eigenaar");
    printf("Naam eigenaar: %s\n", toml_raw_in(eigenaar, "naam"));

    toml_free(conf);
    return 0;
}
```
Voorbeelduitvoer:
```
Titel: "TOML Voorbeeld"
Naam eigenaar: "Tom Preston-Werner"
```

## Diepgaande Verkenning
TOML, wat staat voor Tom's Obvious, Minimal Language, werd in 2013 gecreëerd door Tom Preston-Werner. Het dient als een eenvoudiger alternatief voor formaten zoals XML en YAML, met een focus op meer leesbaarheid en schrijfbaarheid voor mensen. Hoewel JSON een ander alternatief is, behoudt TOML een structuur die visueel gemakkelijker te parseren is door mensen, wat een van de primaire redenen is voor de adoptie in configuratiebestanden.

In C werken met TOML omvat het kiezen van een parserbibliotheek aangezien de taal dit niet native ondersteunt. Bibliotheken zoals "tomlc99" zijn C99-conform en bieden een API om TOML-tekst te decoderen. Bij het overwegen van prestaties zijn goede foutafhandeling en geheugenbeheer cruciaal aangezien C geen ingebouwde vuilnisophaaldienst heeft.

## Zie Ook:
1. TOML Spec: [https://toml.io/en/](https://toml.io/en/)
2. tomlc99 GitHub repo: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. Vergelijking van Gegevensserialisatieformaten: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)
