---
title:                "Travailler avec json"
html_title:           "C: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/working-with-json.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi on le fait?

JSON, ou JavaScript Object Notation, est un format de données très utilisé par les programmeurs pour échanger des données entre différentes applications. Il est léger, facile à lire et à écrire, et est basé sur la syntaxe de JavaScript. Les programmeurs utilisent JSON pour structurer et organiser les données de manière à ce qu'elles puissent être facilement traitées par d'autres programmes.

## Comment faire:

Voici un exemple de code en C pour afficher et parser des données JSON:

```
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "json.h"

int main() {
    // Déclaration d'une chaîne de caractères représentant le JSON
    char* json_string = "{\"nom\": \"Jean\", \"age\": 30}";

    // Déclaration d'une structure pour stocker les données JSON
    json_value* json;

    // Parse de la chaîne de caractères en données JSON
    json = json_parse(json_string, strlen(json_string));

    // Utilisation des données pour afficher le nom et l'âge
    printf("Nom: %s\n", json->u.object.values[0].value->u.string.ptr);
    printf("Age: %d\n", json->u.object.values[1].value->u.integer);

    // Libération de la mémoire allouée pour les données JSON
    json_value_free(json);

    return 0;
}
```

La sortie de ce code sera:

```
Nom: Jean
Age: 30
```

## Plongée en profondeur:

JSON a été développé par Douglas Crockford en 2001 en tant que format de données léger et facile à utiliser pour les applications web. Il est largement utilisé dans le monde de la programmation pour échanger des données entre différents systèmes. Alternatives à JSON incluent XML, CSV et YAML. JSON est souvent utilisé avec les frameworks de développement web tels que JavaScript et PHP. Pour travailler avec JSON en C, vous pouvez utiliser des bibliothèques open-source comme [cJSON](https://github.com/DaveGamble/cJSON) ou [Jansson](https://github.com/akheron/jansson).

## Voir aussi:

- [Documentation officielle de JSON](https://www.json.org/)
- [Tutoriel sur le traitement de JSON en C](https://www.section.io/engineering-education/json-in-c/)