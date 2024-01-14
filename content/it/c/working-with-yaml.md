---
title:                "C: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore C, potresti chiederti perché dovresti imparare a lavorare con YAML. La risposta è semplice: YAML è uno strumento utile per gestire dati strutturati in modo leggibile e facilmente manipolabile. In questo blog, parleremo di come utilizzarlo nel tuo codice C e approfondiremo alcuni aspetti più avanzati.

## Come fare

Prima di tutto, è necessario includere la libreria YAML nella tua applicazione C. Puoi farlo aggiungendo `yaml.h` al tuo file di intestazione e `libyaml` alla lista delle librerie da linkare. Una volta fatto questo, puoi utilizzare le funzioni di YAML per leggere e scrivere dati nei tuoi programmi.

Ecco un esempio di codice che mostra come scrivere un file YAML usando la libreria `libyaml`:

```C
#include <yaml.h>

// Definisci i dati da scrivere nel file YAML
char* nome = "Marco";
int età = 28;
char* lavoro = "Programmatore";

// Apri il file per la scrittura
FILE *f = fopen("dati.yaml", "w");

// Inizializza il tipo di documento, il flusso e il parser YAML
yaml_emitter_t *emitter = malloc(sizeof(yaml_emitter_t));
yaml_event_t *event = malloc(sizeof(yaml_event_t));
yaml_emitter_initialize(emitter);
yaml_emitter_set_output_file(emitter, f);
yaml_emitter_set_canonical(emitter, 1);

// Inizia a scrivere il file YAML
yaml_stream_start_event_initialize(event, YAML_UTF8_ENCODING);
yaml_emitter_emit(emitter, event);
yaml_document_start_event_initialize(event, NULL, NULL, NULL, 1);
yaml_emitter_emit(emitter, event);
yaml_mapping_start_event_initialize(event, NULL, (yaml_char_t *) YAML_MAP_TAG,
  0, YAML_BLOCK_MAPPING_STYLE);
yaml_emitter_emit(emitter, event);

// Scrivi le chiavi e i valori nel tuo documento YAML
yaml_scalar_event_initialize(event, NULL, (yaml_char_t *) "nome", strlen("nome"),
  1, 1, YAML_PLAIN_SCALAR_STYLE);
yaml_emitter_emit(emitter, event);
yaml_scalar_event_initialize(event, NULL, (yaml_char_t *) nome, strlen(nome),
  1, 1, YAML_PLAIN_SCALAR_STYLE);
yaml_emitter_emit(emitter, event);
yaml_scalar_event_initialize(event, NULL, (yaml_char_t *) "età", strlen("età"),
  1, 1, YAML_PLAIN_SCALAR_STYLE);
yaml_emitter_emit(emitter, event);
yaml_scalar_event_initialize(event, NULL, (yaml_char_t *) &età, sizeof(età),
  1, 1, YAML_PLAIN_SCALAR_STYLE);
yaml_emitter_emit(emitter, event);
yaml_scalar_event_initialize(event, NULL, (yaml_char_t *) "lavoro", strlen("lavoro"),
  1, 1, YAML_PLAIN_SCALAR_STYLE);
yaml_emitter_emit(emitter, event);
yaml_scalar_event_initialize(event, NULL, (yaml_char_t *) lavoro, strlen(lavoro),
  1, 1, YAML_PLAIN_SCALAR_STYLE);
yaml_emitter_emit(emitter, event);

// Concludi il file YAML
yaml_mapping_end_event_initialize(event);
yaml_emitter_emit(emitter, event);
yaml_document_end_event_initialize(event, 1);
yaml_emitter_emit(emitter, event);
yaml_stream_end_event_initialize(event);
yaml_emitter_emit(emitter, event);

// Pulisci la memoria e chiudi il file
yaml_emitter_delete(emitter);
yaml_event_delete(event);
fclose(f);
```

Questo codice produrrà il seguente file YAML:

```yaml
---
nome: Marco
età: 28
lavoro: Programmatore
```

Puoi anche leggere un file YAML utilizzando le funzioni `yaml_parser` e `yaml_parser_scan`. Ecco un esempio di codice che legge il file YAML del nostro esempio precedente e stampa i valori delle chiavi:

```C
#include <yaml.h>

// Definisci il puntatore al file
FILE *f;

// Apri il file in modalità lettura
f = fopen("dati.yaml", "r");

// Inizializza il parser YAML e il documento YAML
yaml_parser_t *parser = malloc(sizeof(yaml_parser_t));
yaml_parser_initialize(parser);
yaml_document_t *doc = malloc(sizeof(yaml_document_t));
yaml_parser_set_input_file(parser, f);