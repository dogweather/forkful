---
title:                "Travailler avec yaml"
html_title:           "Fish Shell: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un amateur de programmation, il est fort probable que vous ayez entendu parler de YAML. Cette syntaxe de données est de plus en plus populaire, et est utilisée par de nombreux langages de programmation pour stocker et gérer des données structurées. Dans cet article, nous allons explorer comment utiliser YAML dans votre shell Fish.

## Comment faire

La première étape pour travailler avec YAML dans Fish Shell est d'installer le plugin correspondant. Pour ce faire, vous pouvez utiliser la commande suivante :

```Fish Shell
omf install yaml
```

Une fois le plugin installé, vous pouvez utiliser la commande `yq` pour travailler avec des fichiers YAML. Par exemple, pour afficher le contenu d'un fichier YAML, vous pouvez utiliser la commande suivante :

```Fish Shell
yq read fichier.yaml
```

Pour ajouter une nouvelle entrée dans le fichier YAML, vous pouvez utiliser la commande `yq write` en spécifiant le chemin de la nouvelle clé et sa valeur, comme ceci :

```Fish Shell
yq write fichier.yaml chemin.clé "nouvelle valeur"
```

Vous pouvez également utiliser `grep` pour rechercher des éléments spécifiques dans un fichier YAML :

```Fish Shell
yq read fichier.yaml | grep "élément recherché"
```

## Plongée en profondeur

Maintenant que vous savez comment utiliser les commandes principales pour travailler avec YAML dans Fish Shell, regardons de plus près les différentes manières de modifier et d'analyser des fichiers YAML.

Vous pouvez modifier un fichier YAML en utilisant des expressions régulières avec `sed`. Par exemple, pour remplacer toutes les occurences d'une chaîne de caractères dans un fichier YAML, vous pouvez utiliser la commande suivante :

```Fish Shell
sed -i "s/ancienne_chaîne/nouvelle_chaîne/g" fichier.yaml
```

De plus, si vous avez besoin d'analyser un fichier YAML avec des scripts, vous pouvez utiliser la bibliothèque Python `pyyaml` pour le faire facilement.

## Voir aussi

- [Documentation officielle Fish Shell](https://fishshell.com/docs/current/index.html)
- [Github du plugin YAML pour Fish Shell](https://github.com/fishery/yaml)