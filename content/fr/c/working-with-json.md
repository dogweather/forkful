---
title:                "Manipulation de JSON"
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et pourquoi ?)
JSON, c'est du texte pour stocker et échanger des données. Les programmeurs s'en servent car c'est simple à lire et à écrire, compact et compatible avec le Web.

## How to (Comment faire)
En C, on utilise souvent la bibliothèque 'cJSON'. Un exemple :

```C
#include <stdio.h>
#include "cJSON.h"

int main(){
    // Créer un objet JSON
    cJSON *mon_json = cJSON_CreateObject();
    cJSON_AddItemToObject(mon_json, "nom", cJSON_CreateString("Dupont"));
    cJSON_AddItemToObject(mon_json, "age", cJSON_CreateNumber(30));

    // Imprimer l'objet JSON
    char *json_string = cJSON_Print(mon_json);
    printf("%s\n", json_string);

    // Libérer la mémoire
    cJSON_Delete(mon_json);
    free(json_string);

    return 0;
}
```

Output:
```
{
	"nom": "Dupont",
	"age": 30
}
```

## Deep Dive (Plongée en profondeur)
Créé en 2001, JSON (JavaScript Object Notation) s'est imposé comme un standard de fait grâce à sa simplicité. Des alternatives, comme XML, existent mais sont moins pratiques pour le web moderne. En C, intégrer JSON c'est gérer manuellement la mémoire et respecter des conventions de la bibliothèque utilisée, ici 'cJSON'.

## See Also (Voir aussi)
- Doc de cJSON: https://github.com/DaveGamble/cJSON
- Specs JSON: https://www.json.org/json-fr.html
- Comparaison JSON vs XML: https://www.w3schools.com/js/js_json_xml.asp
