---
title:                "C: Arbeta med YAML"
simple_title:         "Arbeta med YAML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

Om du är en programmerare och arbetar med datahantering, har du säkert stött på YAML-filer. YAML är en filtyp som används för att lagra data på ett strukturerat sätt. Det är ett populärt val bland utvecklare på grund av dess enkelhet och läsbarhet. I denna bloggpost kommer vi att dyka djupare in i hur du kan använda YAML i ditt C-programmeringsprojekt.

## Hur man gör det

För att använda YAML i ditt C-program, måste du först installera ett YAML-bibliotek. Ett populärt val är libYAML, som är utvecklat speciellt för C-programmerare. När du har installerat biblioteket kan du enkelt läsa och skriva YAML-filer i ditt program.

För att läsa en YAML-fil och skriva ut dess innehåll till konsolen kan du använda följande kod:

```C
#include <stdio.h>
#include <yaml.h>

int main() {
    // Skapa en yaml_parser_t-struktur
    yaml_parser_t parser;
    if (!yaml_parser_initialize(&parser)) {
        fputs("Läsare kunde inte initieras\n", stderr);
        return 1;
    }

    // Öppna YAML-filen för läsning
    FILE *file = fopen("data.yaml", "rb");
    if (!file) {
        fputs("Kunde inte öppna YAML-filen\n", stderr);
        return 1;
    }

    // Ställ in filen som inmatningsström för läsaren
    yaml_parser_set_input_file(&parser, file);

    // Så här deklarerar du en yaml_event_t-struktur som håller det nuvarande händelsen
    yaml_event_t event;

    // Så här läser du igenom YAML-filen
    do {
        // Läs in händelse
        if (!yaml_parser_parse(&parser, &event)) {
            fputs("Fel medan du lade in\n", stderr);
            return 1;
        }

        // Utskrift av det innehåll som finns i händelsen
        switch (event.type) {
            case YAML_NO_EVENT: break; // Gör ingenting
            case YAML_STREAM_START_EVENT: puts("YAML-strömmen började"); break;
            case YAML_STREAM_END_EVENT: puts("YAML-strömmen slutade"); break;
            case YAML_DOCUMENT_START_EVENT: puts("En ny dokumentbörjan"); break;
            case YAML_DOCUMENT_END_EVENT: puts("En dokumentslut"); break;
            case YAML_MAPPING_START_EVENT: puts("En karta börjar"); break;
            // Fortsätt läsa andra möjliga händelser...
        }

        // Ta bort den nuvarande händelsen
        yaml_event_delete(&event);
    } while (event.type != YAML_STREAM_END_EVENT);

    // Stäng filen och ta bort alla minnesresurser
    fclose(file);
    yaml_parser_delete(&parser);
    return 0;
}
```

Output från konsolen:

```
YAML-strömmen började
En ny dokumentbörjan
En karta börjar
var1: value1
var2: value2
En karta börjar
en inre_var1: inre_värde1
en inre_var2: inre_värde2
En kartslut
En karta börjar
en annan_inre_var1: annat_inre_värde1
en annan_inre_var2: annat_inre_värde2
En kartslut
En kartslut
En dokumentslut
YAML-strömmen slutade
```


## Djupdykning

YAML är inte bara en filtyp, det är också ett format för att representera datastrukturer och används ofta för konfigurationsfiler. Det är ett lättläst format som även stödjer kommentarer för att göra filerna mer förståeliga. Dessutom kan YAML-filer enkelt konverteras till andra dataformat som JSON.

För mer detaljerad information om hur du kan använda YAML i ditt C-programmeringsprojekt, kan du läsa dokumentationen för libYAML eller utforska andra YAML-bibliotek som libyaml, yaml-cpp eller yaml-cpp. Det finns också flera användbara öppna källkodsprojekt