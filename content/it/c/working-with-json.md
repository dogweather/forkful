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

## Cos'è e perché?

Lavorare con JSON significa manipolare dati in formato JSON, una forma compatta e leggibile di rappresentare dati strutturati. I programmatori utilizzano JSON per scambiare dati tra applicazioni o per memorizzarli su database.

## Come fare:

Utilizzare una libreria C come "json-c" per analizzare ed elaborare dati JSON. Di seguito è riportato un esempio di come leggere un file JSON e estrarre il valore di una chiave:

```C
#include <stdio.h>
#include <json-c/json.h>

int main() {
  struct json_object* json = json_object_from_file("file.json");
  struct json_object* value = json_object_object_get(json, "chiave");
  printf("valore della chiave: %s\n", json_object_to_json_string(value));
  json_object_put(json);
  return 0;
}
```

Output:
```
valore della chiave: "valore della chiave"
```

## Approfondimento:

Il formato JSON è stato inventato da Douglas Crockford nel 2001 ed è diventato uno standard de facto per lo scambio di dati su internet. Alcune alternative al formato JSON sono XML e YAML, ma JSON è preferito per la sua semplicità e leggibilità. Per lavorare con JSON in C, puoi anche utilizzare altre librerie come "cJSON" o "jansson".

## Vedi anche:

- [Libreria json-c] (https://github.com/json-c/json-c)
- [Libreria cJSON] (https://github.com/DaveGamble/cJSON)
- [Libreria Jansson] (https://github.com/akheron/jansson)