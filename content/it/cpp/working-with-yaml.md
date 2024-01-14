---
title:                "C++: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché lavorare con YAML

YAML, acronimo di "YAML Ain't Markup Language", è un formato di file leggibile dall'uomo e facile da interpretare per le macchine. Questo lo rende una scelta popolare per la gestione di dati strutturati in molte applicazioni. Inoltre, è un formato flessibile, che consente di memorizzare dati in un formato facilmente modificabile.

## Come utilizzare YAML in C++

Per utilizzare YAML in C++, la prima cosa da fare è includere la sua libreria nel file di programma. Si può utilizzare un software come "libyaml" per fare ciò. Successivamente, è necessario inizializzare una coppia "yaml_parser_t" e "yaml_composer_t" e impostare la loro struttura. Infine, si può utilizzare la funzione "yaml_parser_parse()" per interpretare il file YAML e ottenere i dati.

Un esempio di codice potrebbe essere il seguente:

```C++
#include <yaml.h>
void read_data_from_yaml()
{
    // Inizializzazione del parser e del compositore
    yaml_parser_t parser;
    yaml_composer_t composer;
    yaml_parser_initialize(&parser);
    yaml_composer_initialize(&composer, 
            &parser, NULL, NULL, NULL);
            
    // Funzione per parsare il file YAML
    int success;
    do {
        // Funzione per ottenere i dati
        success = yaml_parser_parse(&parser, &composer);
    } while(success);

    // Ottenimento dei dati da "composer"
    // e chiusura di entrambi i parser
    yaml_parser_delete(&parser);
    yaml_composer_delete(&composer);
}
```

L'output ottenuto dal file YAML potrebbe essere il seguente:

```C++
ListToBuy:
    - Item1
    - Item2
    - Item3
```

## Approfondimenti su YAML

Se si vuole saperne di più su YAML, è possibile leggere la sua documentazione ufficiale su [yaml.org](https://yaml.org/). Inoltre, è possibile trovare molte risorse online che spiegano come utilizzare questo formato in diverse situazioni. Ad esempio, è possibile approfondire l'utilizzo di YAML con librerie specifiche per altri linguaggi di programmazione, come Python o Java.

## Vedi anche

- [YAML - Documentazione ufficiale](https://yaml.org/)
- [Libreria YAML per C++](https://github.com/jbeder/yaml-cpp)
- [Utilizzo di YAML in Python con PyYAML](https://pyyaml.org/)
- [Utilizzo di YAML in Java con SnakeYAML](https://bitbucket.org/asomov/snakeyaml)