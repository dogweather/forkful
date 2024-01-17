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

Vad är YAML och varför ska du använda det?

YAML står för "YAML Ain't Markup Language" (YAML är inte märkspråk) och det är ett sätt att strukturera data. Programmmers använder YAML för att göra sina kodfiler läsbara och organisera data på ett enkelt sätt.

Så här gör du:

```C
#include <stdio.h>
#include <yaml.h>

int main() {

  FILE *fp;
  yaml_parser_t parser;
  yaml_event_t event;

  fp = fopen("example.yml", "rb");
  yaml_parser_initialize(&parser);
  yaml_parser_set_input_file(&parser, fp);

  do {
    yaml_parser_parse(&parser, &event);
    printf("Event type: %d\n", event.type);
    yaml_event_delete(&event);
  } while(event.type != YAML_STREAM_END_EVENT);

  yaml_parser_delete(&parser);
  fclose(fp);

  return 0;
}
```

Djupdykning:

YAML skapades 2001 av Clark Evans och Ingy döt Net och var ursprungligen utformat för programmeringsspråk som Perl och Python. Det är ett alternativ till XML och JSON för att strukturera data. YAML är också ett vanligt sätt att konfigurera program och webbtjänster.

Se även:

- YAML officiella hemsida: https://yaml.org/
- Jämförelse mellan YAML och XML: https://www.w3schools.com/xml/xml_vs_yaml.asp
- YAML-tutorial för nybörjare: https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/