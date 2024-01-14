---
title:                "C: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

JSON (JavaScript Object Notation) est un format de données léger et simple pour échanger des informations entre différentes applications. En utilisant JSON, vous pouvez facilement stocker et transmettre des données structurées telles que des chaînes de caractères, des nombres et des objets complexes.

## Comment faire

Si vous êtes nouveau dans la programmation C, voici un exemple simple pour créer et lire des données JSON :

```C
#include <stdio.h>
#include <stdlib.h>
#include <json-c/json.h>

int main()
{
    // Crée un objet JSON vide
    struct json_object *my_object = json_object_new_object();

    // Ajoute une chaîne de caractères à l'objet avec la clé "nom"
    json_object_object_add(my_object, "name", json_object_new_string("Jean"));

    // Ajoute un nombre à l'objet avec la clé "age"
    json_object_object_add(my_object, "age", json_object_new_int(25));

    // Affiche l'objet JSON au format string
    printf("%s\n", json_object_to_json_string(my_object));

    // Libère la mémoire allouée pour l'objet
    json_object_put(my_object);

    return 0;
}
```

Output :

```json
{"name": "Jean", "age": 25}
```

Vous pouvez également lire un fichier JSON existant en utilisant la fonction `json_object_from_file()` et accéder à ses valeurs en utilisant les fonctions `json_object_object_get()` et `json_object_get_*()` selon le type de données.

Pour plus d'exemples et de fonctions utiles, vous pouvez consulter la documentation de la bibliothèque json-c.

## Plongée en profondeur

En travaillant avec JSON en C, il est important de comprendre que les données doivent être converties en objets JSON à l'aide de la bibliothèque, et vice versa pour les récupérer. De plus, JSON ne prend pas en charge les commentaires, il est donc important de vérifier la validité des données avant de les utiliser.

La bibliothèque json-c offre de nombreuses fonctionnalités, telles que la création d'objets JSON à partir de chaînes de caractères, la mise en forme et la validation des données, et même la possibilité de manipuler les données à l'intérieur des objets sans avoir à les extraire complètement.

## Voir aussi

- [Documentation json-c](https://json-c.github.io/json-c/)
- [Exemples de manipulation JSON en C](https://github.com/json-c/json-c/blob/master/tests/json_object.c)
- [Tutoriel JSON en français](https://zestedesavoir.com/tutoriels/326/introduction-a-json/)

Merci d'avoir lu cet article sur le travail avec JSON en C. En utilisant cette bibliothèque, vous pourrez facilement manipuler des données JSON dans vos applications et les échanger avec d'autres en toute simplicité. Bon codage !