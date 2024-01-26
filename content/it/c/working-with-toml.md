---
title:                "Lavorare con TOML"
date:                  2024-01-26T04:19:29.537802-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con TOML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/working-with-toml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
TOML è un linguaggio di serializzazione dei dati progettato per essere facile da leggere e scrivere. I programmatori lo utilizzano per file di configurazione, semplice memorizzazione dei dati e scambio di dati tra diversi linguaggi, grazie alla sua chiarezza e amichevolezza per l'utente.

## Come fare:
Analizziamo un file di configurazione TOML in C utilizzando la libreria "tomlc99". Prima, installa la libreria. Poi, crea un `config.toml`:

```toml
title = "Esempio TOML"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
```

Ora, analizziamolo in C:

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("Errore: impossibile aprire il file di configurazione\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("Errore: %s\n", errbuf);
        return 1;
    }

    printf("Titolo: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* owner = toml_table_in(conf, "owner");
    printf("Nome Proprietario: %s\n", toml_raw_in(owner, "name"));

    toml_free(conf);
    return 0;
}
```
Output di esempio:
```
Titolo: "Esempio TOML"
Nome Proprietario: "Tom Preston-Werner"
```

## Approfondimento
TOML, che sta per Tom's Obvious, Minimal Language, è stato creato da Tom Preston-Werner nel 2013. Serve come alternativa più semplice a formati come XML e YAML, concentrando sulla facilità di lettura e scrittura da parte degli umani. Anche se JSON è un'altra alternativa, TOML mantiene una struttura che è più facile da analizzare visivamente per gli umani, che è uno dei motivi principali della sua adozione nei file di configurazione.

In C, lavorare con TOML comporta la scelta di una libreria di analisi poiché il linguaggio non lo supporta nativamente. Librerie come "tomlc99" sono conformi a C99 e forniscono un'API per decodificare il testo TOML. Quando si considera la performance, è cruciale una corretta gestione degli errori e della memoria poiché C non ha un raccoglitore di spazzatura integrato.

## Vedi Anche:
1. Specifica TOML: [https://toml.io/en/](https://toml.io/en/)
2. Repository GitHub tomlc99: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. Confronto dei formati di serializzazione dei dati: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)