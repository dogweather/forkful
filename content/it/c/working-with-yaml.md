---
title:                "Lavorare con YAML"
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
YAML è un formato per strutturare dati, facile da scrivere e leggere per gli umani. I programmatori lo usano per configurazioni, file di dati e in applicazioni che richiedono serializzazione/deserializzazione di dati complessi.

## Come Fare:
C non ha supporto nativo per YAML, quindi utilizzeremo la libreria `libyaml` per esempi di parsing/scrivitura yaml. 

Esempio di Parsing YAML in C:

```c
#include <stdio.h>
#include <yaml.h>

int main() {
    FILE *fh = fopen("config.yaml", "r");
    yaml_parser_t parser;
    yaml_event_t event;

    yaml_parser_initialize(&parser);
    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event)) {
            printf("Errore nel parsing del file YAML.\n");
            exit(EXIT_FAILURE);
        }

        if (event.type == YAML_STREAM_END_EVENT) break;

        // Gestire gli eventi qui...

        yaml_event_delete(&event);
    }

    yaml_parser_delete(&parser);
    fclose(fh);
    return 0;
}
```

Supponendo `config.yaml` sia:

```yaml
version: 1
path: "/usr/local/bin"
enabled: true
```

Questo esempio legge il yaml ma non fa nulla con i dati. Puoi espandere la gestione degli eventi per costruire strutture dati.

## Approfondimento
YAML nasce agli inizi del 2000 come alternativa più leggibile a XML e JSON. Le alternative includono appunto JSON, XML o INI per configurazioni più semplici. L'implementazione di YAML in C richiede la gestione manuale della memoria e la comprensione degli eventi di parsing per tradurre YAML in strutture dati C.

## Vedi Anche

- [libyaml GitHub](https://github.com/yaml/libyaml) - Per il codice sorgente e documentazione.
- [La specifica YAML](https://yaml.org/spec/1.2/spec.html) - Per imparare a fondo il formato YAML.
- [Tutorial di libyaml](https://github.com/yaml/libyaml/wiki/Tutorial) - Per approfondimenti su come usare la libyaml.
- [Documentazione YAML per sviluppatori C](https://pyyaml.org/wiki/LibYAML) - Per guide su libyaml in C.
