---
title:                "Lavorare con JSON"
date:                  2024-01-19
simple_title:         "Lavorare con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/working-with-json.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Lavorare con JSON (JavaScript Object Notation) significa manipolare dati strutturati, utili per lo scambio di informazioni tra client e server. Lo usiamo perché è leggero, facile da leggere per l'uomo e da analizzare per le macchine.

## Come fare:
Supponiamo di voler leggere un JSON e stampare il valore di una chiave specifica. Useremo la libreria `cJSON`:

```C
#include <stdio.h>
#include "cjson/cJSON.h"

int main() {
    // Esempio di JSON
    char *my_json_string = "{\"name\":\"Mario\",\"age\":30}";

    // Parse del JSON
    cJSON *parsed_json = cJSON_Parse(my_json_string);
    
    // Estrazione del valore della chiave "name"
    cJSON *name = cJSON_GetObjectItemCaseSensitive(parsed_json, "name");
    
    if (cJSON_IsString(name) && (name->valuestring != NULL)) {
        printf("Name: %s\n", name->valuestring);
    }
    
    // Pulizia
    cJSON_Delete(parsed_json);
    
    return 0;
}
```
Output:
```
Name: Mario
```

## Approfondimenti
Historicamente, JSON nasce nei primi anni 2000 come alternativa a XML, meno verboso e più semplice da manipolare. Altre alternative includono YAML o BSON. Implementare il supporto JSON in C richiede l'uso di librerie esterne come `cJSON` o `Jansson`, che forniscono funzioni per il parsing e la generazione di JSON.

## Vedi anche
- Documentazione di `cJSON`: https://github.com/DaveGamble/cJSON
- Comparazione tra librerie JSON in C: https://en.wikibooks.org/wiki/C_Programming/JSON
- Tutorial JSON in C con libreria `Jansson`: http://www.digip.org/jansson/
