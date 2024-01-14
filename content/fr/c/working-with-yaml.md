---
title:                "C: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi travailler avec YAML?

Si vous êtes un programmeur qui travaille avec des données structurées, vous avez probablement entendu parler de YAML. YAML, sigle pour YAML Ain't Markup Language, est un format de données basé sur le langage de balisage léger qui est largement utilisé dans le monde du développement logiciel. Si vous n'êtes pas encore familier avec YAML, cet article vous expliquera pourquoi il est utile et comment l'utiliser.

## Comment utiliser YAML en langage C

Pour utiliser YAML en langage C, vous devrez d'abord inclure la bibliothèque de code YAML dans votre code. Vous pouvez le faire avec l'instruction ```C #include <yaml.h>```. Ensuite, vous devez initialiser la bibliothèque en utilisant l'instruction ```C yaml_parser_t parser; yaml_parser_initialize(&parser);```.

Ensuite, vous pourrez charger votre fichier YAML à l'aide de l'instruction ```C FILE *yamlfile = fopen("exemple.yaml", "rb");```. Assurez-vous de remplacer "exemple.yaml" par le nom de votre propre fichier YAML. Enfin, vous pouvez lire et écrire des données YAML en utilisant les fonctions de la bibliothèque YAML.

Voici un exemple de code utilisant la bibliothèque YAML en langage C pour créer un fichier YAML et y écrire des données:

```C
#include <stdio.h>
#include <yaml.h>

int main()
{
  // Initialiser la bibliothèque YAML
  yaml_parser_t parser;
  yaml_parser_initialize(&parser);

  // Ouvrir le fichier YAML en mode écriture
  FILE *yamlfile = fopen("utilisateurs.yaml", "w");

  // Créer un noeud racine pour notre document YAML
  yaml_event_t event;
  event.type = YAML_DOCUMENT_START_EVENT;
  yaml_parser_emit(&parser, &event);

  // Ajouter un noeud "utilisateurs"
  event.type = YAML_MAPPING_START_EVENT;
  // Notez les indentations dans le code, elles sont importantes
  yaml_parser_emit(&parser, &event);

  // Ajouter un noeud "nom" avec la valeur "Jean"
  event.type = YAML_SCALAR_EVENT;
  event.data.scalar.value = "nom";
  yaml_parser_emit(&parser, &event);

  event.type = YAML_SCALAR_EVENT;
  event.data.scalar.value = "Jean";
  yaml_parser_emit(&parser, &event);

  // Ajouter un noeud "âge" avec la valeur 25
  event.type = YAML_SCALAR_EVENT;
  event.data.scalar.value = "âge";
  yaml_parser_emit(&parser, &event);

  event.type = YAML_SCALAR_EVENT;
  event.data.scalar.value = "25";
  yaml_parser_emit(&parser, &event);

  // Terminer le noeud "utilisateurs"
  event.type = YAML_MAPPING_END_EVENT;
  yaml_parser_emit(&parser, &event);

  // Terminer le document YAML
  event.type = YAML_DOCUMENT_END_EVENT;
  yaml_parser_emit(&parser, &event);

  // Arrêter et fermer le parser et le fichier YAML
  yaml_parser_delete(&parser);
  fclose(yamlfile);

  return 0;
}
```

Le code ci-dessus créera un fichier YAML appelé "utilisateurs.yaml" avec le contenu suivant:

```yaml
utilisateurs:
  nom: Jean
  âge: 25
```

## Plongée en profondeur dans YAML

En plus d'être un langage de balisage léger, YAML est également un langage de sérialisation de données. Cela signifie qu'il est utile pour stocker des données complexes dans un format facile à lire et à écrire. Les données YAML peuvent être représentées sous forme de listes, de tableaux, de dictionnaires et bien plus encore, ce qui en fait un choix populaire pour les fichiers de configuration de logiciels.

Une autre caractéristique importante de YAML est son interface simple et flexible. Elle permet également de définir des types personnalisés pour les données, offrant ainsi un contrôle plus précis sur la façon dont les données sont stockées et traitées.

Il est important de noter que YAML peut être sensible à l'indentation et aux espaces blancs. Il est donc important de respecter les conventions de mise en forme pour éviter les erreurs lors de la lecture et de l'écriture de données YAML.

## Voir aussi

- [Documentation officielle de YAML](https://yaml.org/)
- [Tutoriel sur YAML en langage C](https://