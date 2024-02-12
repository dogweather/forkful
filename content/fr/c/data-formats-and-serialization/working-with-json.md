---
title:                "Travailler avec JSON"
aliases:
- /fr/c/working-with-json.md
date:                  2024-02-03T18:11:51.325832-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Travailler avec JSON (JavaScript Object Notation) en C implique l'analyse, la génération, et la manipulation des structures de données JSON. Les programmeurs font cela pour permettre la communication avec les services web, le stockage de données, ou les fichiers de configuration dans un format allégé et lisible par l'homme.

## Comment :

Pour travailler avec JSON en C, vous utiliserez généralement une bibliothèque comme `jansson` ou `json-c` en raison du manque de support intégré pour JSON en C. Ici, nous nous concentrerons sur `jansson` pour sa facilité d'utilisation et sa maintenance active. Tout d'abord, installez la bibliothèque (par exemple, en utilisant un gestionnaire de paquets comme `apt` sur Ubuntu : `sudo apt-get install libjansson-dev`).

Commençons par analyser une chaîne JSON et accéder à son contenu :

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    const char *json_string = "{\"name\":\"John Doe\",\"age\":30}";
    json_error_t error;
    json_t *root = json_loads(json_string, 0, &error);
    
    if(!root) {
        fprintf(stderr, "erreur: à la ligne %d: %s\n", error.line, error.text);
        return 1;
    }
    
    const char *name;
    int age;
    json_unpack(root, "{s:s, s:i}", "name", &name, "age", &age);
    
    printf("Nom : %s\nÂge : %d\n", name, age);
    
    json_decref(root);
    return 0;
}
```

Exemple de sortie :
```
Nom : John Doe
Âge : 30
```

Ensuite, la création et l'affichage d'un objet JSON :

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

Exemple de sortie :
```
{"name": "Jane Doe", "age": 25}
```

Ces exemples démontrent les bases du chargement d'une chaîne JSON, du déballage de ses valeurs, de la création d'un nouvel objet JSON et ensuite de son affichage sous forme de chaîne.

## Plongée Profonde

La nécessité de travailler avec JSON en C provient de l'adoption du JSON par le web comme format principal pour l'échange de données. La simplicité et l'efficacité de JSON l'ont rapidement fait surpasser XML, malgré l'absence initiale en C d'un support direct pour la manipulation de JSON. Les premières solutions impliquaient une manipulation manuelle des chaînes - sujette aux erreurs et inefficace. Des bibliothèques comme `jansson` et `json-c` sont apparues pour combler ce vide, offrant des API robustes pour l'analyse, la construction, et la sérialisation de JSON.

Tandis que `jansson` offre simplicité et facilité d'utilisation, `json-c` pourrait séduire ceux qui recherchent un ensemble de fonctionnalités plus large. Néanmoins, des alternatives comme les bibliothèques d'analyse en C++ offrent des abstractions plus sophistiquées, grâce aux structures de données plus complexes de ce langage et au support de la bibliothèque standard. Cependant, lorsqu'on travaille dans des environnements où le C est la langue préférée ou requise - comme dans les systèmes embarqués ou lors de l'interface avec des bibliothèques C existantes - l'utilisation de `jansson` ou `json-c` devient indispensable.

Il convient également de noter que travailler avec JSON en C implique une compréhension plus approfondie de la gestion de la mémoire, car ces bibliothèques retournent fréquemment des objets alloués dynamiquement nécessitant une désallocation explicite. Cela défie les programmeurs de trouver un équilibre entre commodité et responsabilité de prévenir les fuites de mémoire, un aspect crucial de la rédaction de code C efficace.
