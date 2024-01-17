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

## Qu'est-ce que c'est et pourquoi le faire?

Travailler avec YAML est le moyen pour les programmeurs de stocker des données dans un format facilement lisible par les humains et les machines. Il peut être utilisé pour configurer des applications, enregistrer des données de configuration ou même créer des documents structurés.

## Comment faire:

```
#include <stdio.h>

int main() {
    FILE *file = fopen("example.yml", "w");
    if (file == NULL) {
        printf("Erreur lors de l'ouverture du fichier.");
        return 1;
    }

    fprintf(file, "nom: Jean\nage: 25\nville: Paris\n");
    fclose(file);

    return 0;
}
```

La sortie sera un fichier example.yml contenant:

```YAML
nom: Jean
age: 25
ville: Paris
```

## Plongez plus profondément:

YAML a été créé en 2001 et signifie "YAML Ain't Markup Language". Il a été conçu pour être un remplacement plus facile à lire et à écrire que XML. Cependant, il peut également être utilisé en conjonction avec JSON et d'autres formats de données.

Il existe d'autres alternatives à YAML telles que JSON, TOML et INI. Cependant, YAML est souvent préféré pour sa simplicité et sa syntaxe flexible.

La mise en œuvre de YAML se fait via des bibliothèques telles que libyaml, yaml-cpp ou yaml-c. Ces bibliothèques fournissent des fonctions et des méthodes pour lire et écrire des fichiers YAML.

## Voir aussi:

- [Site officiel de YAML](https://yaml.org/)
- [Tutoriel pour débuter avec YAML](https://learnxinyminutes.com/docs/yaml/)
- [Comparaison de YAML avec d'autres formats de données](https://stackoverflow.com/questions/1726802/what-is-a-good-choice-for-a-simple-document-format-that-is-easily-routable-in-j)