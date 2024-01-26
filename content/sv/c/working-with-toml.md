---
title:                "Att arbeta med TOML"
date:                  2024-01-26T04:19:41.566985-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med TOML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/working-with-toml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
TOML är ett dataserieringsspråk som är utformat för att vara lättläst och lättskrivet. Programmerare använder det för konfigurationsfiler, enkel datalagring och utbyte av data mellan olika programmeringsspråk på grund av dess tydlighet och användarvänlighet.

## Hur gör man:
Låt oss tolka en TOML-konfigurationsfil i C med hjälp av biblioteket "tomlc99". Först, installera biblioteket. Skapa sedan en `config.toml`:

```toml
title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
```

Tolka den nu i C:

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("Error: cannot open config file\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("Error: %s\n", errbuf);
        return 1;
    }

    printf("Title: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* owner = toml_table_in(conf, "owner");
    printf("Owner Name: %s\n", toml_raw_in(owner, "name"));

    toml_free(conf);
    return 0;
}
```
Exempel på utskrift:
```
Title: "TOML Example"
Owner Name: "Tom Preston-Werner"
```

## Fördjupning
TOML, som står för Toms Obvious, Minimal Language, skapades av Tom Preston-Werner år 2013. Det fungerar som ett enklare alternativ till format som XML och YAML, med fokus på att vara mer läsbart och skrivbart för människor. Även om JSON är ett annat alternativ, behåller TOML en struktur som är lättare att visuellt tolka av människor, vilket är en av de främsta anledningarna till dess användning i konfigurationsfiler.

I C innebär arbete med TOML att välja ett tolkbibliotek eftersom språket inte stöder det inbyggt. Bibliotek som "tomlc99" är förenliga med C99 och tillhandahåller ett API för att avkoda TOML-text. När man överväger prestanda, är korrekt felhantering och minneshantering avgörande eftersom C inte har inbyggd skräpinsamling.

## Se också:
1. TOML Spec: [https://toml.io/en/](https://toml.io/en/)
2. tomlc99 GitHub repo: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. Jämförelse av dataserieringsformat: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)