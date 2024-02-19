---
aliases:
- /nl/c/working-with-toml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:20.276525-07:00
description: "TOML (Tom's Obvious, Minimal Language) is een configuratiebestandsformaat\
  \ dat makkelijk te lezen is door zijn duidelijke semantiek. Programmeurs gebruiken\u2026"
lastmod: 2024-02-18 23:09:02.399224
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) is een configuratiebestandsformaat\
  \ dat makkelijk te lezen is door zijn duidelijke semantiek. Programmeurs gebruiken\u2026"
title: Werken met TOML
---

{{< edit_this_page >}}

## Wat & Waarom?

TOML (Tom's Obvious, Minimal Language) is een configuratiebestandsformaat dat makkelijk te lezen is door zijn duidelijke semantiek. Programmeurs gebruiken het voor configuratiebestanden in applicaties omdat de eenvoud en leesbaarheid door mensen het een uitstekende keuze maken over formaten zoals XML of JSON in bepaalde contexten.

## Hoe te:

Om met TOML in C te werken, heb je eerst een bibliotheek nodig die in staat is om TOML-bestanden te parseren, aangezien de C-standaardbibliotheek deze functionaliteit niet bevat. Een populaire keuze is `tomlc99`, een lichtgewicht TOML-parser voor C99. Hier is een snelle gids om een eenvoudig TOML-configuratiebestand te lezen:

Eerst, zorg ervoor dat je `tomlc99` hebt geïnstalleerd en correct gelinkt in je project.

**Voorbeeld TOML-bestand (`config.toml`):**
```toml
[database]
server = "192.168.1.1"
poorten = [ 8001, 8001, 8002 ]
max_aantal_verbindingen = 5000
ingeschakeld = true
```

**C-code om dit bestand te parsen:**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("Kan bestand niet openen");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "Fout bij het parsen van bestand\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("Databaseserver: %s\n", server);

        toml_array_t *poorten = toml_array_in(database, "poorten");
        for (int i = 0; i < toml_array_nelem(poorten); i++) {
            int64_t poort;
            toml_int_at(poorten, i, &poort);
            printf("Poort %d: %ld\n", i, poort);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**Uitvoer:**
```
Databaseserver: "192.168.1.1"
Poort 0: 8001
Poort 1: 8001
Poort 2: 8002
```

## Diepgaande duik

TOML is gecreëerd door Tom Preston-Werner, mede-oprichter van GitHub, als een reactie op de beperkingen die hij waarnam in andere configuratiebestandsformaten. Het doel is om rechttoe rechtaan en ondubbelzinnig te zijn, zowel voor mensen als computers, om te lezen en te schrijven zonder complexe parsingregels nodig te hebben. In het C-ecosysteem is TOML geen eerste klas burger zoals het misschien in hogere programmeertalen zoals Rust met zijn `serde_toml` of Python met `toml` is, die bibliotheken hebben met native ondersteuning. Echter, C-ontwikkelaars moeten vertrouwen op externe bibliotheken zoals `tomlc99`, maar dit is typerend gezien de nadruk van C op minimalisme en prestatie.

Hoewel TOML wordt geprezen voor zijn duidelijkheid, is het bij het kiezen van een configuratiebestandsformaat essentieel om de behoeften van het project te overwegen. In scenario's die complexere structuren vereisen of interactiviteit met web-API's, kunnen JSON of zelfs YAML ondanks hun verhoogde complexiteit een betere pasvorm bieden. TOML schittert in configuraties waar leesbaarheid en eenvoud vooropstaan, niet noodzakelijkerwijs waar de meest geavanceerde gegevensstructuren nodig zijn.
