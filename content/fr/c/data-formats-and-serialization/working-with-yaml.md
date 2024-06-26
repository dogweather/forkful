---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:08.756299-07:00
description: "Comment faire : Travailler avec YAML en C n\xE9cessite une biblioth\xE8\
  que, car la biblioth\xE8que standard du C ne fournit pas de support direct pour\
  \ l'analyse ou\u2026"
lastmod: '2024-03-13T22:44:58.394886-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec YAML en C n\xE9cessite une biblioth\xE8que, car la biblioth\xE8\
  que standard du C ne fournit pas de support direct pour l'analyse ou la s\xE9rialisation\
  \ de YAML."
title: Travailler avec YAML
weight: 41
---

## Comment faire :
Travailler avec YAML en C nécessite une bibliothèque, car la bibliothèque standard du C ne fournit pas de support direct pour l'analyse ou la sérialisation de YAML. L'une des bibliothèques YAML les plus populaires pour C est `libyaml`, qui offre des interfaces de bas et haut niveau pour l'analyse et l'émission de YAML. Voici un exemple de la manière de parser un simple fichier YAML en utilisant `libyaml` :

**Premièrement**, vous devez installer la bibliothèque `libyaml`. Si vous êtes sur un système de type Unix, vous pouvez généralement l'installer via votre gestionnaire de paquets. Par exemple, sur Ubuntu :

```bash
sudo apt-get install libyaml-dev
```

**Ensuite**, considérez un simple fichier YAML nommé `config.yaml` :

```yaml
nom: John Doe
âge: 29
marié: faux
```

**Voici** un exemple basique de comment parser ce fichier YAML en C :

```c
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

void traiter_fichier_yaml(const char *nom_fichier) {
    FILE *fh = fopen(nom_fichier, "rb");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("Échec de l'initialisation du parseur YAML !\n", stderr);

    if (fh == NULL)
        fputs("Échec de l'ouverture du fichier !\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        if (event.type == YAML_SCALAR_EVENT) {
            printf("Valeur : %s\n", event.data.scalar.value);
        }

        if (event.type == YAML_STREAM_END_EVENT)
            break;

        yaml_event_delete(&event);
    }

    yaml_parser_delete(&parser);
    fclose(fh);
}

int main() {
    traiter_fichier_yaml("config.yaml");
    return 0;
}
```

Ce programme simple ouvre un fichier YAML, initialise le parseur YAML et lit le fichier, en imprimant les valeurs scalaires (dans cet exemple, les champs de notre simple YAML). Notez que la vérification des erreurs est minimale dans cet exemple simple et devrait être plus robuste dans un code de production.

Exécuter le programme avec notre `config.yaml` produira :

```plaintext
Valeur : John Doe
Valeur : 29
Valeur : faux
```

## Plongeon en profondeur
YAML a été lancé pour la première fois en 2001 et a été conçu pour être plus lisible et convivial que d'autres formats de sérialisation de données tels que XML ou JSON, empruntant à plusieurs langues comme C, Perl et Python pour sa philosophie de conception. Malgré ses avantages en termes de lisibilité et de facilité de modification humaine, YAML peut être complexe à parser de manière programmatique en raison de sa dépendance à l'indentation et de son ensemble de fonctionnalités étendu, incluant les références et les types personnalisés.

Bien que `libyaml` offre un accès robuste et de bas niveau pour l'analyse et l'émission de YAML en C, cela peut être fastidieux pour des tâches simples en raison de son API verbeuse. Pour ces raisons, certains programmeurs préfèrent utiliser des bibliothèques de plus haut niveau ou même d'autres formats de sérialisation de données comme JSON lorsqu'ils travaillent en C, en particulier lorsque l'analyse performante avec un minimum de surcharge de code est une priorité. Cependant, YAML reste un choix populaire pour les fichiers de configuration et les situations où la lisibilité humaine est primordiale. Des alternatives comme TinyYAML ou l'intégration d'un interpréteur de haut niveau (par exemple, l'intégration de Python ou Lua) pourraient offrir plus de commodité pour des applications spécifiques, équilibrant entre facilité d'utilisation et besoins en performance.
