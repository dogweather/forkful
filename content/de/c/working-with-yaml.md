---
title:                "Arbeiten mit YAML"
date:                  2024-01-19
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML, "YAML Ain't Markup Language", ist ein Datenformat zum leicht lesbaren Austausch von Daten. Programmierer nutzen es für Konfigurationsdateien und Datenübertragung wegen seiner Klarheit und Einfachheit.

## How to:
In C gibt es keine integrierte YAML-Unterstützung, also nutzt man Bibliotheken wie `libyaml`. Hier ein Beispiel für das Einlesen einer YAML-Datei:

```C
#include <stdio.h>
#include <yaml.h>

int main(void){
    FILE *fh = fopen("config.yaml", "r");
    yaml_parser_t parser;
    yaml_token_t  token;

    if(!yaml_parser_initialize(&parser))
        fputs("Failed to initialize parser!\n", stderr);
    if(fh == NULL)
        fputs("Failed to open file!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    do {
        yaml_parser_scan(&parser, &token);
        switch(token.type)
        {
        /* Token-Verarbeitung hier */
        }
    } while(token.type != YAML_STREAM_END_TOKEN);

    yaml_token_delete(&token);
    yaml_parser_delete(&parser);
    fclose(fh);
    return 0;
}
```

Beispiel für config.yaml:
```yaml
version: 1
path: "/usr/local/bin"
enabled: true
```

Sample Output ist hier nicht sinnvoll, da das Lesen von YAML-Dateien im Wesentlichen im Reagieren auf Tokens besteht.

## Deep Dive
YAML entstand Anfang der 2000er als human-freundliches Datenformat. Alternativen sind JSON und XML, aber YAML ist oft lesbarer. Die Komplexität von YAML ergibt sich aus der Unterstützung vielfältiger Datentypen und Strukturen. C mit YAML zu verwenden, erfordert meist eine externe Bibliothek, wie `libyaml` oder `yaml-cpp` für C++.

## See Also
- YAML offizielle Seite: https://yaml.org
- `libyaml` – C-Bibliothek für YAML: https://github.com/yaml/libyaml
- YAML Syntax: https://yaml.org/spec/1.2/spec.html
- Ein Tutorial für YAML in C: https://www.wpsoftware.net/andrew/pages/libyaml.html
