---
title:                "Arbeiten mit TOML"
date:                  2024-01-26T04:19:15.847720-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit TOML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-toml.md"
---

{{< edit_this_page >}}

## Was & Warum?
TOML ist eine Daten-Serialisierungssprache, die entworfen wurde, um leicht les- und schreibbar zu sein. Programmierer nutzen sie für Konfigurationsdateien, einfache Datenspeicherung und Datenaustausch zwischen verschiedenen Sprachen wegen ihrer Klarheit und Benutzerfreundlichkeit.

## Wie:
Lassen Sie uns eine TOML-Konfigurationsdatei in C mithilfe der Bibliothek "tomlc99" parsen. Installieren Sie zuerst die Bibliothek. Erstellen Sie dann eine `config.toml`:

```toml
title = "TOML Beispiel"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
```

Nun, parsen Sie es in C:

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

    printf("Titel: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* owner = toml_table_in(conf, "owner");
    printf("Eigentümer Name: %s\n", toml_raw_in(owner, "name"));

    toml_free(conf);
    return 0;
}
```
Beispielausgabe:
```
Titel: "TOML Beispiel"
Eigentümer Name: "Tom Preston-Werner"
```

## Vertiefung
TOML, die Abkürzung für Toms Offensichtliche, Minimale Sprache, wurde 2013 von Tom Preston-Werner erstellt. Es dient als eine einfachere Alternative zu Formaten wie XML und YAML, mit dem Fokus auf einer besseren Les- und Schreibbarkeit für Menschen. Während JSON eine weitere Alternative ist, behält TOML eine Struktur bei, die von Menschen visuell einfacher zu parsen ist, was einer der Hauptgründe für seine Anwendung in Konfigurationsdateien ist.

In C mit TOML zu arbeiten, erfordert die Auswahl einer Parser-Bibliothek, da die Sprache dies nicht nativ unterstützt. Bibliotheken wie "tomlc99" entsprechen dem C99-Standard und bieten eine API zur Dekodierung von TOML-Texten. Wenn man die Leistung betrachtet, sind eine ordnungsgemäße Fehlerbehandlung und Speicherverwaltung entscheidend, da C keine integrierte automatische Speicherbereinigung hat.

## Siehe auch:
1. TOML Spezifikation: [https://toml.io/en/](https://toml.io/en/)
2. tomlc99 GitHub-Repo: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. Vergleich von Daten-Serialisierungsformaten: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)