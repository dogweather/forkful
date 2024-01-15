---
title:                "Lavorare con yaml"
html_title:           "C: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore C e stai cercando un modo semplice per gestire file di configurazione o dati strutturati, allora lavorare con YAML potrebbe essere la soluzione ideale per te.

## Come fare

Per iniziare a lavorare con YAML in C, è necessario avere a disposizione una libreria che supporti questo formato. Fortunatamente, esiste una libreria popolare ed efficiente chiamata libyaml. Puoi installarla utilizzando il gestore dei pacchetti del tuo sistema operativo o scaricandola direttamente dal suo repository GitHub.

Una volta installata, puoi includere la libreria nel tuo codice C e iniziare a manipolare facilmente i dati YAML. Ecco un semplice esempio di come leggere un file YAML e accedere ai suoi elementi:

```
#include <yaml.h>

int main() {
    // apre il file YAML
    FILE *file = fopen("config.yaml", "rb");

    // inizializza il parser YAML
    yaml_parser_t parser;
    yaml_parser_initialize(&parser);

    // associa il file al parser
    yaml_parser_set_input_file(&parser, file);

    // inizializza la variabile di supporto per il documento YAML
    yaml_document_t document;
    memset(&document, 0, sizeof(yaml_document_t));

    // legge il documento YAML
    yaml_parser_load(&parser, &document);

    // accede al primo elemento nel documento
    yaml_node_t *node = yaml_document_get_root_node(&document);

    // accede al valore della chiave "nome"
    yaml_node_t *nome = yaml_document_get_node(&document, node->data.mapping.pairs->key);

    // stampa il valore
    printf("Il nome nel file YAML è %s", nome->data.scalar.value);

    // pulisce la memoria
    yaml_document_delete(&document);
    yaml_parser_delete(&parser);
    fclose(file);

    return 0;
}
```

Output:

```
Il nome nel file YAML è John
```

Ovviamente, questo è solo un esempio di base per mostrare come utilizzare la libreria libyaml. Ci sono molti altri metodi e funzioni disponibili per gestire i dati YAML in modo più approfondito.

## Approfondimento

Per saperne di più sui dettagli di YAML e su come utilizzarlo al meglio nel tuo codice C, è consigliato leggere la specifica ufficiale del formato disponibile sul sito ufficiale del progetto YAML.

Inoltre, è possibile consultare la documentazione completa della libreria libyaml per avere una visione più approfondita delle funzionalità disponibili e dei loro utilizzi.

## Vedi anche

- [Sito ufficiale di YAML](https://yaml.org/)
- [Specifiche YAML](https://yaml.org/spec/)
- [Documentazione di libyaml](https://github.com/yaml/libyaml)