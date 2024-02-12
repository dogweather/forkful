---
title:                "Lavorare con JSON"
aliases: - /it/c/working-with-json.md
date:                  2024-02-03T18:11:51.548451-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

Lavorare con JSON (JavaScript Object Notation) in C implica l'analisi, la generazione e la manipolazione delle strutture dati JSON. I programmatori fanno ciò per abilitare la comunicazione con i servizi web, l'archiviazione dei dati o i file di configurazione in un formato leggero e leggibile dall'uomo.

## Come fare:

Per lavorare con JSON in C, di solito si utilizza una libreria come `jansson` o `json-c` a causa della mancanza di supporto integrato per JSON in C. Qui, ci concentreremo su `jansson` per la sua facilità di uso e manutenzione attiva. Prima di tutto, installa la libreria (ad esempio, usando un gestore di pacchetti come `apt` su Ubuntu: `sudo apt-get install libjansson-dev`).

Iniziamo analizzando una stringa JSON e accedendo ai suoi contenuti:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    const char *json_string = "{\"name\":\"John Doe\",\"age\":30}";
    json_error_t error;
    json_t *root = json_loads(json_string, 0, &error);
    
    if(!root) {
        fprintf(stderr, "errore: alla linea %d: %s\n", error.line, error.text);
        return 1;
    }
    
    const char *name;
    int age;
    json_unpack(root, "{s:s, s:i}", "name", &name, "age", &age);
    
    printf("Nome: %s\nEtà: %d\n", name, age);
    
    json_decref(root);
    return 0;
}
```

Esempio di output:
```
Nome: John Doe
Età: 30
```

Successivamente, creazione e output di un oggetto JSON:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    json_t *root = json_object();
    json_object_set_new(root, "name", json_string("Jane Doe"));
    json_object_set_new(root, "age", json_integer(25));
    
    char *json_dump = json_dumps(root, JSON_ENCODE_ANY);
    printf("%s\n", json_dump);
    
    free(json_dump);
    json_decref(root);
    return 0;
}
```

Esempio di output:
```
{"name": "Jane Doe", "age": 25}
```

Questi esempi dimostrano le basi del caricamento di una stringa JSON, disimballaggio dei suoi valori, creazione di un nuovo oggetto JSON e poi la sua output come stringa.

## Approfondimento

La necessità di lavorare con JSON in C nasce dall'adozione del web di JSON come formato primario per lo scambio di dati. La semplicità e l'efficienza di JSON lo hanno rapidamente fatto superare XML, nonostante l'assenza iniziale in C del supporto diretto per la manipolazione JSON. Le prime soluzioni implicavano la manipolazione manuale delle stringhe - propensa agli errori e inefficiente. Biblioteche come `jansson` e `json-c` sono emerse per colmare questa lacuna, fornendo robuste API per l'analisi, la costruzione e la serializzazione di JSON.

Mentre `jansson` offre semplicità e facilità di uso, `json-c` potrebbe attirare coloro che cercano un set di funzionalità più ampio. Tuttavia, alternative come le librerie di analisi in C++ offrono astrazioni più sofisticate, grazie alle strutture dati più complesse di quel linguaggio e al supporto della libreria standard. Tuttavia, quando si lavora in ambienti in cui C è il linguaggio preferito o richiesto - come nei sistemi embedded o quando si interfaccia con le librerie C esistenti - l'uso di `jansson` o `json-c` diventa indispensabile.

Vale anche la pena notare che lavorare con JSON in C comporta una comprensione più approfondita della gestione della memoria, poiché queste librerie restituiscono spesso oggetti allocati dinamicamente che richiedono una deallocata esplicita. Questo sfida i programmatori a bilanciare la convenienza con la responsabilità di prevenire le perdite di memoria, un aspetto cruciale della creazione di codice C efficiente.
