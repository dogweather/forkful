---
title:                "Werken met YAML"
date:                  2024-01-28T22:11:48.681183-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

YAML is een voor mensen leesbaar gegevensserialisatieformaat dat wordt gebruikt voor configuratiebestanden, gegevensuitwisseling tussen talen en gegevensopslag. Programmeurs kiezen voor YAML vanwege de eenvoud en leesbaarheid, waardoor het een fluitje van een cent is om te gebruiken voor snelle configuratie- en ontwikkelingstaken.

## Hoe:

C heeft geen ingebouwde YAML-parser, dus we gebruiken een bibliotheek zoals `libyaml` om YAML-bestanden te verwerken. Hier is een eenvoudig voorbeeld van het parseren van een YAML-bestand in C.

Eerst de bibliotheek includen:
```C
#include <yaml.h>
```

Vervolgens een parser initialiseren, een bestand openen en beginnen met parseren:
```C
FILE *fh = fopen("config.yaml", "r");
yaml_parser_t parser;
yaml_parser_initialize(&parser);
yaml_parser_set_input_file(&parser, fh);

yaml_event_t event;
/* De eventreeks lezen */
while (true) {
    if (!yaml_parser_parse(&parser, &event)) {
        printf("Parserfout %d\n", parser.error);
        exit(EXIT_FAILURE);
    }

    if (event.type == YAML_SCALAR_EVENT) {
        printf("Kreeg scalar (waarde): %s\n", event.data.scalar.value);
    }

    if (event.type == YAML_STREAM_END_EVENT) {
        break;
    }

    yaml_event_delete(&event);
}

/* Opruimen */
yaml_parser_delete(&parser);
fclose(fh);
```

Voorbeeldinhoud `config.yaml`:
```yaml
name: John Doe
age: 30
```

Voorbeelduitvoer:
```
Kreeg scalar (waarde): name
Kreeg scalar (waarde): John Doe
Kreeg scalar (waarde): age
Kreeg scalar (waarde): 30
```

## Diepere Duik

YAML staat voor "YAML Ain't Markup Language." Het is begin jaren 2000 ontstaan als alternatief voor XML voor configuratiebestanden, met als doel menselijke leesbaarheid. YAML wordt gebruikt in veel tools (zoals Docker, Kubernetes, enz.) en wordt vaak verkozen boven JSON voor configuraties vanwege de ondersteuning voor commentaren en schonere syntaxis.

Veelgebruikte C-alternatieven voor het werken met YAML zijn `libyaml` en `yaml-cpp` (hoewel de laatste voor C++ is). Deze bibliotheken stellen C/C++-programma's in staat om YAML-gegevens te serialiseren en te deserialiseren.

Bij het parsen van YAML bouwt uw programma een boom in het geheugen op. Knopen in deze boom kunnen mappings (zoals woordenboeken of hash-tabellen), sequenties (zoals arrays) of scalairs (strings, getallen, enz.) zijn. De parser van libyaml is event-gedreven, wat betekent dat het de YAML-stream leest en events uitgeeft voor elke aangetroffen YAML-structuur. Het afhandelen van deze events stelt u in staat om de overeenkomstige gegevensstructuur te bouwen of ermee te werken.

## Zie Ook

- `libyaml` GitHub: https://github.com/yaml/libyaml
- YAML officiële specificaties: https://yaml.org/spec/1.2/spec.html
- "Programmeren met libyaml" tutorial: https://libyaml.docsforge.com/master/programming-with-libyaml/
- Vergelijking van gegevensserialisatieformaten: https://nl.wikipedia.org/wiki/Comparison_of_data-serialization_formats
