---
title:                "C: Lavorare con JSON"
simple_title:         "Lavorare con JSON"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/working-with-json.md"
---

{{< edit_this_page >}}

## Perché lavorare con JSON

Se sei un programmatore C alla ricerca di uno strumento versatile per la manipolazione dei dati, allora lavorare con JSON potrebbe essere la soluzione perfetta per te. JSON, acronimo di JavaScript Object Notation, è un formato leggero e ampiamente utilizzato per lo scambio di dati tra applicazioni. È facile da leggere e scrivere, ed è supportato da una vasta gamma di linguaggi di programmazione, rendendolo un'ottima scelta per lavorare con dati strutturati.

## Come lavorare con JSON in C

Per lavorare con JSON in C, è necessario includere la libreria <stdio.h> e <jansson.h> nel tuo codice. Successivamente, puoi creare un oggetto JSON e riempirlo con i tuoi dati mediante l'utilizzo delle funzioni offerte dalla libreria jansson. Ecco un esempio di codice C per creare e stampare un oggetto JSON:

```
#include <stdio.h>
#include <jansson.h>

int main() {

    // Creazione di un oggetto JSON vuoto
    json_t *obj = json_object();
    // Aggiunta di un valore intero all'oggetto
    json_object_set_new(obj, "numero", json_integer(42));
    // Aggiunta di una stringa all'oggetto
    json_object_set_new(obj, "nome", json_string("Mario Rossi"));
    // Stampa dell'oggetto JSON
    char *output = json_dumps(obj, JSON_INDENT(4));
    printf("%s\n", output);
    
    return 0;
}

```

Questo codice produrrà l'output seguente:

```
{
    "numero": 42,
    "nome": "Mario Rossi"
}
```

## Approfondimento su JSON

Per chi vuole saperne di più su JSON, ci sono diverse risorse online disponibili per imparare a lavorare con esso in modo più approfondito. Uno dei migliori modi per imparare è quello di seguire dei tutorial pratici come quello offerto da W3Schools (https://www.w3schools.com/js/js_json_intro.asp). Inoltre, è possibile consultare la documentazione ufficiale della libreria jansson (http://jansson.readthedocs.io/en/latest/) per una guida dettagliata su tutte le funzioni disponibili.

## Vedi anche

- Tutorial di W3Schools su JSON: https://www.w3schools.com/js/js_json_intro.asp
- Documentazione ufficiale di jansson: http://jansson.readthedocs.io/en/latest/