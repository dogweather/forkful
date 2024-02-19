---
aliases:
- /nl/c/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:38.715801-07:00
description: "YAML, wat staat voor \"YAML Ain't Markup Language\", is een voor mensen\
  \ leesbare standaard voor gegevensserialisatie die voor allerlei toepassingen kan\u2026"
lastmod: 2024-02-18 23:09:02.396024
model: gpt-4-0125-preview
summary: "YAML, wat staat voor \"YAML Ain't Markup Language\", is een voor mensen\
  \ leesbare standaard voor gegevensserialisatie die voor allerlei toepassingen kan\u2026"
title: Werken met YAML
---

{{< edit_this_page >}}

## Wat & Waarom?

YAML, wat staat voor "YAML Ain't Markup Language", is een voor mensen leesbare standaard voor gegevensserialisatie die voor allerlei toepassingen kan worden gebruikt, van configuratiebestanden tot gegevensopslag. Programmeurs werken vaak met YAML wanneer ze een makkelijk te lezen en te schrijven formaat nodig hebben voor configuratiebestanden of gegevensuitwisseling tussen talen en systemen.

## Hoe te:

Werken met YAML in C vereist een bibliotheek, aangezien de standaard C-bibliotheek geen directe ondersteuning biedt voor YAML-parsing of -serialisatie. Een van de meest populaire YAML-bibliotheken voor C is `libyaml`, die zowel low-level als high-level interfaces biedt voor het parsen en uitzenden van YAML. Hieronder staat een voorbeeld van hoe je een eenvoudig YAML-bestand kunt parsen met `libyaml`:

**Ten eerste**, moet je de `libyaml`-bibliotheek installeren. Als je op een op Unix lijkend systeem zit, kun je het meestal via je pakketbeheerder installeren. Bijvoorbeeld, op Ubuntu:

```bash
sudo apt-get install libyaml-dev
```

**Vervolgens**, beschouw een eenvoudig YAML-bestand genaamd `config.yaml`:

```yaml
name: John Doe
age: 29
married: false
```

**Hier is** een basisvoorbeeld van hoe dit YAML-bestand in C te parsen:

```c
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

void process_yaml_file(const char *filename) {
    FILE *fh = fopen(filename, "rb");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("YAML-parser initialiseren mislukt!\n", stderr);

    if (fh == NULL)
        fputs("Kan bestand niet openen!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        if (event.type == YAML_SCALAR_EVENT) {
            printf("Waarde: %s\n", event.data.scalar.value);
        }

        if (event.type == YAML_STREAM_END_EVENT)
            break;

        yaml_event_delete(&event);
    }

    yaml_parser_delete(&parser);
    fclose(fh);
}

int main() {
    process_yaml_file("config.yaml");
    return 0;
}
```

Dit eenvoudige programma opent een YAML-bestand, initialiseert de YAML-parser en leest het bestand, waarbij de scalairwaarden worden afgedrukt (in dit voorbeeld, de velden van onze eenvoudige YAML). Merk op dat de foutcontrole minimaal is in dit eenvoudige voorbeeld en robuuster zou moeten zijn in productiecode.

Het uitvoeren van het programma met onze `config.yaml` zal uitvoeren:

```plaintext
Waarde: John Doe
Waarde: 29
Waarde: false
```

## Diepere Duik

YAML werd voor het eerst uitgebracht in 2001 en is ontworpen om leesbaarder en gebruiksvriendelijker te zijn dan andere gegevensserialisatieformaten zoals XML of JSON, waarbij het leent van verschillende talen zoals C, Perl en Python voor zijn ontwerpfilosofie. Ondanks de voordelen in leesbaarheid en gemak van menselijke aanpassing, kan YAML complex zijn om programmatisch te parsen vanwege het beroep op inspringing en de uitgebreide functionaliteit, inclusief verwijzingen en aangepaste typen.

Hoewel `libyaml` robuuste, low-level toegang biedt tot het parsen en uitzenden van YAML in C, kan het omslachtig zijn voor eenvoudige taken vanwege de uitgebreide API. Om deze redenen geven sommige programmeurs de voorkeur aan het gebruik van hogere-niveau bibliotheken of zelfs andere gegevensserialisatieformaten zoals JSON, wanneer ze in C werken, vooral wanneer performante parsing met minimale code overhead een prioriteit is. Echter, YAML blijft een populaire keuze voor configuratiebestanden en situaties waar menselijke leesbaarheid van het grootste belang is. Alternatieven zoals TinyYAML of het inbedden van een high-level interpreter (bijv. het embedden van Python of Lua) kunnen meer gemak bieden voor specifieke toepassingen, een evenwicht zoekend tussen gebruiksgemak en prestatiebehoeften.
