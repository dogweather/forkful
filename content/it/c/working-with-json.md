---
title:                "Lavorare con json"
html_title:           "C: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Se stai lavorando con dati strutturati in C, è probabile che prima o poi ti imbatterai nel formato JSON. Questo formato è diventato sempre più popolare negli ultimi anni ed è essenziale per la scambio di dati tra diverse applicazioni.

## Come fare

Per lavorare con JSON in C, hai bisogno di utilizzare una libreria esterna. Una delle più famose è jansson, che offre una vasta gamma di funzioni per manipolare, creare e analizzare dati JSON. Ecco un esempio di codice per creare un oggetto JSON:

```
#include <jansson.h>
#include <stdio.h>

int main() {
    //Crea un oggetto JSON vuoto
    json_t *obj = json_object();

    //Aggiunge valori all'oggetto
    json_object_set_new(obj, "nome", json_string("Mario"));
    json_object_set_new(obj, "eta", json_integer(25));
    json_object_set_new(obj, "hobby", json_array());
    json_array_append_new(json_object_get(obj, "hobby"), json_string("tennis"));
    json_array_append_new(json_object_get(obj, "hobby"), json_string("musica"));

    //Stampa l'oggetto JSON
    printf("%s\n", json_dumps(obj, JSON_INDENT(2)));

    //Rilascia la memoria
    json_decref(obj);

    return 0;
}
```

L'output di questo codice sarà il seguente:

```
{
  "nome": "Mario",
  "eta": 25,
  "hobby": [
    "tennis",
    "musica"
  ]
}
```

Puoi anche utilizzare jansson per analizzare dati JSON da una stringa o da un file, scrivere dati JSON in un file o per gestire errori durante la manipolazione di dati JSON. E' importante consultare la documentazione ufficiale per tutte le funzioni disponibili e per saperne di più sulle possibilità offerte dalla libreria.

## Approfondimento

Il formato JSON è basato su due strutture di base: oggetti e array. Gli oggetti sono collezioni non ordinate di coppie chiave-valore, mentre gli array sono collezioni ordinate di valori. Uno dei vantaggi di JSON è che può essere facilmente convertito in una struttura dati C, rendendo la manipolazione dei dati più semplice.

E' importante tenere a mente che alcune caratteristiche di C, come i float o le stringhe, non sono direttamente supportate da JSON. In questi casi, è necessario convertire manualmente il dato al tipo supportato da JSON prima di analizzarlo o di crearne uno nuovo.

Utilizzare una libreria esterna per manipolare i dati JSON può essere un po' scomodo, ma ti offre una serie di funzionalità utili che lo rendono uno strumento essenziale per gestire dati strutturati in C.

## Vedi anche

- Documentazione ufficiale di jansson: http://www.digip.org/jansson/
- Tutorial di JSON in C: https://www.geeksforgeeks.org/json-simple-api-in-c/