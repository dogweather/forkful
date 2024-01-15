---
title:                "Att arbeta med yaml"
html_title:           "C: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför
 Om du är en programmerare som arbetar i C, kan du undra varför du skulle vilja använda YAML (YAML Ain't Markup Language). Svaret är enkelt: YAML är ett lättläst och lättförståeligt format för att lagra och överföra data. Det gör det perfekt för konfigurationsfiler, dataöverföring mellan program eller för att spara information för senare återanvändning.

## Hur man
För att använda YAML i dina C-program behöver du först inkludera "libyaml" biblioteket. Sedan kan du börja läsa och skriva YAML-data med hjälp av funktioner som `yaml_parser_parse` och `yaml_parser_emit` inuti "```C ... ```" kodblock. Här är ett exempel på hur du skulle läsa in en YAML-fil och skriva ut dess innehåll:

```C
// Inkluderar libyaml biblioteket
#include <yaml.h>
// Skapar en parser för att läsa in YAML-filen
yaml_parser_t parser;
yaml_parser_initialize(&parser);
// Öppnar YAML-filen
FILE *yaml_file = fopen("data.yml", "r");
// Sätter filen som inmatning för parsern
yaml_parser_set_input_file(&parser, yaml_file);
// Skapar en händelseshanterare för att bearbeta YAML-data
yaml_event_t event;
// Loopar igenom alla händelser i YAML-filen
while (yaml_parser_parse(&parser, &event)) {
  // Skriver ut varje händelse
  printf("%s\n", yaml_dump_event(&event));
  // Frigör minnet som används av händelsen
  yaml_event_delete(&event);
}
// Stänger YAML-filen
fclose(yaml_file);
// Avslutar parsern
yaml_parser_delete(&parser);
```

Exempelutmatningen skulle se ut så här:

```YAML
%YAML 1.2
---
titles:
  - Best C Programming Books
  - The C Programming Language
authors:
  - Stanley B. Lippman
  - Brian W. Kernighan
---
publisher: -
  name: Addison-Wesley Professional
  location: Boston
format: E-Book
isbn: "9780133086218"
```

## Djupdykning
 Nu när du har fått en grundläggande förståelse för hur man läser och skriver YAML-data i C, kan vi ta en djupare titt på några andra funktioner och användningsområden för YAML i C-programmering. En användbar funktion är `yaml_document_get_root_node` som säkerställer att du läser in och arbetar med den korrekta rotnoden i en YAML-fil. En annan fördel med YAML är att det är ett plattforms- och programmeringsspråks-agnostiskt format, vilket innebär att du kan använda det för att dela och överföra data mellan olika program och system.

## Se också
För mer information och resurser om att arbeta med YAML i C-programmering, kolla in följande länkar:

- Libyaml dokumentation: https://pyyaml.org/wiki/LibYAML
- YAML-specifikationen: https://yaml.org/spec/
- YAML för C-bibliotek: https://github.com/yaml/libyaml