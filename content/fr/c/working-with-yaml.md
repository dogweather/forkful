---
title:                "Travailler avec yaml"
html_title:           "C: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes programmeur en C, vous avez probablement déjà rencontré le format YAML dans vos projets. Mais savez-vous pourquoi il est devenu si populaire ? YAML est un format de données flexible et facile à comprendre, idéal pour stocker et échanger des informations entre différentes applications. Il est également très similaire à la syntaxe du C, ce qui le rend très prisé des développeurs de ce langage.

## Comment faire

Pour travailler avec le format YAML en C, vous aurez besoin d'une bibliothèque externe appelée "libyaml". Il existe différentes façons de l'installer, mais la méthode la plus simple est d'utiliser un gestionnaire de paquets tel que "apt" sous Linux. Une fois installée, vous pouvez importer cette bibliothèque dans votre code avec la ligne suivante :

```
#include <yaml.h>
```

Ensuite, pour utiliser la fonctionnalité de parsing de YAML, vous utiliserez principalement les fonctions "yaml_parser_t" et "yaml_parser_parse". Voici un exemple pour illustrer le processus :

```
// Création de la structure de données pour stocker le contenu YAML
typedef struct data {
    int id;
    char name[50];
} data_t;

// Initialisation du parser
yaml_parser_t parser;
yaml_parser_initialize(&parser);

// Ouverture du fichier YAML
FILE *file = fopen("fichier.yaml", "r");

// Configuration du buffer d'entrée
yaml_parser_set_input_file(&parser, file);

// Définition des types de données que nous allons récupérer
yaml_event_t event;
data_t data;

// Lecture du contenu du fichier YAML
while (yaml_parser_parse(&parser, &event)) {
    // Vérification du type d'événement
    if (event.type == YAML_SEQUENCE_START_EVENT) {
        // Début d'une nouvelle séquence de données
        // Nous lisons les données en utilisant la fonction "yaml_parser_parse_scalar"
        yaml_parser_parse_scalar(&parser, &event, &data.id);
        yaml_parser_parse_scalar(&parser, &event, data.name);
        // Maintenant, nous pouvons utiliser les données stockées dans notre structure
        printf("ID : %d, Nom : %s\n", data.id, data.name);
    }
}

// Libération de la mémoire et fermeture du fichier
yaml_event_delete(&event);
yaml_parser_delete(&parser);
fclose(file);
```

Et voilà ! Vous pouvez désormais facilement lire le contenu d'un fichier YAML dans votre programme C.

## Plongée en profondeur

Pour aller plus loin dans l'utilisation de YAML en C, vous pouvez également vous intéresser à la fonctionnalité de écriture de données dans un fichier YAML. Pour cela, vous utiliserez les fonctions "yaml_emitter_t" et "yaml_emitter_dump". Vous pouvez également consulter la documentation complète de la bibliothèque "libyaml" pour découvrir toutes ses possibilités.

## Voir aussi

- [Site officiel de YAML](https://yaml.org/)
- [Documentation de libyaml](https://pyyaml.org/wiki/LibYAML)
- [Exemple de projet utilisant YAML et C](https://github.com/hhartford/C-YAML/tree/master/src)