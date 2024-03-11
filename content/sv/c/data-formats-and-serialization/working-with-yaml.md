---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:43.226419-07:00
description: "YAML, som st\xE5r f\xF6r \"YAML Ain't Markup Language\", \xE4r en standard\
  \ f\xF6r serialisering av data som \xE4r l\xE4sbar f\xF6r m\xE4nniskor och kan anv\xE4\
  ndas f\xF6r alla m\xF6jliga\u2026"
lastmod: '2024-03-11T00:14:11.810285-06:00'
model: gpt-4-0125-preview
summary: "YAML, som st\xE5r f\xF6r \"YAML Ain't Markup Language\", \xE4r en standard\
  \ f\xF6r serialisering av data som \xE4r l\xE4sbar f\xF6r m\xE4nniskor och kan anv\xE4\
  ndas f\xF6r alla m\xF6jliga\u2026"
title: Att arbeta med YAML
---

{{< edit_this_page >}}

## Vad & Varför?

YAML, som står för "YAML Ain't Markup Language", är en standard för serialisering av data som är läsbar för människor och kan användas för alla möjliga applikationer, från konfigurationsfiler till datalagring. Programmerare arbetar ofta med YAML när de behöver ett format som är lätt att läsa och skriva för konfigurationsfiler eller datadelning mellan språk och system.

## Hur:

Att arbeta med YAML i C kräver ett bibliotek, eftersom det standardiserade C-biblioteket inte ger direkt stöd för tolkning eller serialisering av YAML. Ett av de mest populära YAML-biblioteken för C är `libyaml`, som erbjuder både låg- och högnivågränssnitt för att tolka och skapa YAML. Nedan är ett exempel på hur man tolkar en enkel YAML-fil med hjälp av `libyaml`:

**Först**, du behöver installera `libyaml`-biblioteket. Om du är på ett Unix-liknande system kan du vanligtvis installera det via din pakethanterare. Till exempel, på Ubuntu:

```bash
sudo apt-get install libyaml-dev
```

**Nästa**, tänk dig en enkel YAML-fil som heter `config.yaml`:

```yaml
name: John Doe
age: 29
married: false
```

**Här är** ett grundläggande exempel på hur du tolkar denna YAML-fil i C:

```c
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

void process_yaml_file(const char *filename) {
    FILE *fh = fopen(filename, "rb");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("Misslyckades med att initialisera YAML-tolkaren!\n", stderr);

    if (fh == NULL)
        fputs("Misslyckades med att öppna fil!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        if (event.type == YAML_SCALAR_EVENT) {
            printf("Värde: %s\n", event.data.scalar.value);
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

Detta enkla program öppnar en YAML-fil, initialiserar YAML-tolkaren och läser filen, skriver ut skalärvärdena (i detta exempel, fälten i vår enkla YAML). Observera att felkontrollen är minimal i detta enkla exempel och bör vara mer robust i produktionskod.

Kör programmet med vår `config.yaml` kommer att ge utskriften:

```plaintext
Värde: John Doe
Värde: 29
Värde: false
```

## Fördjupning

YAML släpptes för första gången 2001 och var designat för att vara mer läsbart och användarvänligt än andra data serialiseringsformat som XML eller JSON, och lånar från flera språk som C, Perl och Python för sin designfilosofi. Trots sina fördelar i läsbarhet och enkelhet för mänsklig modifiering, kan YAML vara komplext att tolka programmatiskt på grund av sitt beroende av indrag och dess omfattande funktionssätt, inklusive referenser och anpassade typer.

Medan `libyaml` tillhandahåller robust, låg-nivå tillgång till tolkning och skapande av YAML i C, kan det vara besvärligt för enkla uppgifter på grund av dess ordrika API. Av dessa skäl föredrar vissa programmerare att använda högnivåbibliotek eller till och med andra data serialiseringsformat som JSON när de arbetar i C, särskilt när prestanda för tolkning med minimal kodöverhead är en prioritet. Dock förblir YAML ett populärt val för konfigurationsfiler och situationer där människans läsbarhet är av största vikt. Alternativ som TinyYAML eller inbäddning av en högnivåtolk (t.ex. inbäddning av Python eller Lua) skulle kunna erbjuda mer bekvämlighet för specifika applikationer, som balanserar mellan användarvänlighet och prestandabehov.
