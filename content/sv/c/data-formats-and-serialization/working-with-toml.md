---
aliases:
- /sv/c/working-with-toml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:36.154562-07:00
description: "TOML (Toms Uppenbara, Minimala Spr\xE5k) \xE4r ett konfigurationsfilformat\
  \ som \xE4r l\xE4tt att l\xE4sa p\xE5 grund av dess tydliga semantik. Programmerare\
  \ anv\xE4nder det\u2026"
lastmod: 2024-02-18 23:08:52.275271
model: gpt-4-0125-preview
summary: "TOML (Toms Uppenbara, Minimala Spr\xE5k) \xE4r ett konfigurationsfilformat\
  \ som \xE4r l\xE4tt att l\xE4sa p\xE5 grund av dess tydliga semantik. Programmerare\
  \ anv\xE4nder det\u2026"
title: Att Arbeta med TOML
---

{{< edit_this_page >}}

## Vad & Varför?

TOML (Toms Uppenbara, Minimala Språk) är ett konfigurationsfilformat som är lätt att läsa på grund av dess tydliga semantik. Programmerare använder det för konfigurationsfiler i applikationer eftersom dess enkelhet och läsbarhet för människor gör det till ett utmärkt val över format som XML eller JSON i vissa sammanhang.

## Hur man gör:

För att arbeta med TOML i C behöver du först ett bibliotek som kan tolka TOML-filer, eftersom C:s standardbibliotek inte inkluderar denna funktionalitet. Ett populärt val är `tomlc99`, en lättvikts-TOML-tolk för C99. Här är en snabbguide för att läsa en enkel TOML-konfigurationsfil:

Först, se till att du har `tomlc99` installerat och korrekt länkat i ditt projekt.

**Exempel på TOML-fil (`config.toml`):**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**C-kod för att tolka denna fil:**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("Kan inte öppna filen");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "Fel vid tolkning av filen\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("Databasserver: %s\n", server);

        toml_array_t *ports = toml_array_in(database, "ports");
        for (int i = 0; i < toml_array_nelem(ports); i++) {
            int64_t port;
            toml_int_at(ports, i, &port);
            printf("Port %d: %ld\n", i, port);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**Output:**
```
Databasserver: "192.168.1.1"
Port 0: 8001
Port 1: 8001
Port 2: 8002
```

## Fördjupning

TOML skapades av Tom Preston-Werner, medgrundare av GitHub, som ett svar på de begränsningar han upplevde med andra konfigurationsfilformat. Dess mål är att vara rakt på sak och otvetydig, både för människor och datorer, att läsa och skriva utan att behöva komplexa tolkningsregler. I C-ekosystemet är TOML inte en förstklassig medborgare som det kanske är i högre språknivåer som Rust med dess `serde_toml` eller Python med `toml`, vilka har bibliotek med inbyggt stöd. Istället behöver C-utvecklare lita på externa bibliotek som `tomlc99`, men detta är typiskt med tanke på C:s betoning på minimalism och prestanda.

Medan TOML hyllas för sin klarhet, är det viktigt att överväga projektets behov när man väljer ett konfigurationsfilformat. I scenarier som kräver mer komplexa strukturer eller interaktivitet med webb-API:er kan JSON eller till och med YAML erbjuda en bättre passform trots deras ökade komplexitet. TOML utmärker sig i konfigurationer där läsbarhet och enkelhet är av största vikt, inte nödvändigtvis där de mest avancerade datatstrukturerna behövs.
