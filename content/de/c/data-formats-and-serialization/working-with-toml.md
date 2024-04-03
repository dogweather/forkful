---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:10.965767-07:00
description: "Wie geht das: Um mit TOML in C zu arbeiten, ben\xF6tigen Sie zun\xE4\
  chst eine Bibliothek, die in der Lage ist, TOML-Dateien zu parsen, da die\u2026"
lastmod: '2024-03-13T22:44:54.379039-06:00'
model: gpt-4-0125-preview
summary: "Um mit TOML in C zu arbeiten, ben\xF6tigen Sie zun\xE4chst eine Bibliothek,\
  \ die in der Lage ist, TOML-Dateien zu parsen, da die C-Standardbibliothek diese\
  \ Funktionalit\xE4t nicht enth\xE4lt."
title: Arbeiten mit TOML
weight: 39
---

## Wie geht das:
Um mit TOML in C zu arbeiten, benötigen Sie zunächst eine Bibliothek, die in der Lage ist, TOML-Dateien zu parsen, da die C-Standardbibliothek diese Funktionalität nicht enthält. Eine beliebte Wahl ist `tomlc99`, ein leichtgewichtiger TOML-Parser für C99. Hier ist eine kurze Anleitung zum Lesen einer einfachen TOML-Konfigurationsdatei:

Stellen Sie zunächst sicher, dass Sie `tomlc99` installiert und in Ihrem Projekt richtig verlinkt haben.

**Beispiel für eine TOML-Datei (`config.toml`):**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**C-Code zum Parsen dieser Datei:**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("Kann Datei nicht öffnen");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "Fehler beim Parsen der Datei\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("Datenbankserver: %s\n", server);

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

**Ausgabe:**
```
Datenbankserver: "192.168.1.1"
Port 0: 8001
Port 1: 8001
Port 2: 8002
```

## Tiefergehender Einblick
TOML wurde von Tom Preston-Werner, dem Mitbegründer von GitHub, als Reaktion auf die Einschränkungen, die er in anderen Konfigurationsdateiformaten wahrgenommen hat, erstellt. Sein Ziel ist es, unkompliziert und eindeutig zu sein, sowohl für Menschen als auch für Computer, zum Lesen und Schreiben, ohne komplexe Parsing-Regeln zu benötigen. Im C-Ökosystem ist TOML nicht so etabliert wie es vielleicht in höheren Sprachen wie Rust mit seinem `serde_toml` oder Python mit `toml` ist, die Bibliotheken mit nativer Unterstützung haben. Stattdessen müssen C-Entwickler auf externe Bibliotheken wie `tomlc99` zurückgreifen, was typisch ist angesichts der Betonung von C auf Minimalismus und Leistung.

Obwohl TOML für seine Klarheit gelobt wird, ist es von entscheidender Bedeutung, die Bedürfnisse des Projekts zu berücksichtigen, wenn man ein Konfigurationsdateiformat wählt. In Szenarien, die komplexere Strukturen erfordern oder mit Web-APIs interagieren, könnten JSON oder sogar YAML trotz ihrer erhöhten Komplexität besser passen. TOML glänzt in Konfigurationen, wo Lesbarkeit und Einfachheit von größter Bedeutung sind, nicht unbedingt dort, wo die fortschrittlichsten Datenstrukturen benötigt werden.
