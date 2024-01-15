---
title:                "Travailler avec le json"
html_title:           "C: Travailler avec le json"
simple_title:         "Travailler avec le json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur C et que vous travaillez souvent avec des données structurées, vous devez savoir comment travailler avec le format JSON. JSON (JavaScript Object Notation) est un format de données léger et facile à utiliser, qui est largement utilisé dans les applications web et mobiles. En apprenant à utiliser JSON en C, vous pourrez facilement accéder, manipuler et échanger des données avec d'autres applications.

## Comment faire

Pour commencer à travailler avec JSON en C, vous devez inclure le fichier d'en-tête ```<stdlib.h>``` et utiliser la fonction ```malloc()``` pour allouer de l'espace mémoire pour vos données JSON.

```C
#include <stdlib.h>

// Allouer de l'espace mémoire pour un objet JSON
char* json_object = (char*) malloc(sizeof(char) * 50); // 50 étant la taille maximale de l'objet JSON

// Remplir l'objet avec des données (un exemple simplifié)
strcpy(json_object, "{\"nom\": \"Jean\", \"age\": 25}");

// Pour afficher l'objet JSON
printf("%s\n", json_object);
```

La sortie pour l'exemple ci-dessus devrait être: ```{"nom": "Jean", "age": 25}```

Pour accéder aux valeurs spécifiques dans un objet JSON, vous pouvez utiliser la fonction ```strstr()``` pour trouver la clé correspondant à la valeur que vous recherchez, puis utilisrez la fonction ```atoi()``` ou ```atof()``` pour convertir la valeur en entier ou en flottant.

```C
// Trouver la clé "age" dans l'objet JSON
char* key = "age";
char* age_ptr = strstr(json_object, key);

// Convertir la valeur "age" en entier
int age = atoi(age_ptr);

// Afficher la valeur
printf("L'age de %s est de %d ans.\n", json_object, age);
```

La sortie pour cet exemple devrait être: ```L'age de Jean est de 25 ans.```

## Deep Dive

Travailler avec des données JSON en C peut sembler un peu intimidant au début, mais une fois que vous comprenez les bases, vous pourrez facilement interagir avec n'importe quelles données structurées. En plus des fonctions mentionnées précédemment, il existe d'autres fonctions utiles telles que ```strtod()``` pour la conversion de chaînes de caractères en nombres décimaux, et ```strcpy()``` pour la copie de chaînes de caractères. De plus, il est important de comprendre les différentes structures de données disponibles en C, telles que les tableaux et les structures, pour mieux manipuler les données JSON complexes.

## Voir aussi

- [Documentation officielle de JSON en C](https://github.com/json-c/json-c)
- [Tutoriel vidéo sur l'utilisation de JSON en C](https://www.youtube.com/watch?v=y8zi7N0jtJk)
- [Article sur la manipulation de données JSON en C++](https://www.freecodecamp.org/news/how-to-work-with-json-in-cpp/)