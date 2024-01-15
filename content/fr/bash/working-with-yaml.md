---
title:                "Travailler avec yaml"
html_title:           "Bash: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez souvent avec des données structurées, il y a de fortes chances que vous ayez entendu parler de YAML. C'est un format de sérialisation de données qui facilite la manipulation et l'échange de données entre différents programmes. Dans cet article, nous allons explorer les bases de YAML en utilisant Bash comme langage de programmation.

## Comment faire

Pour commencer à utiliser YAML dans vos scripts Bash, vous devez tout d'abord vous assurer que vous avez la dernière version de Bash installée sur votre système. Ensuite, vous pouvez utiliser l'outil de ligne de commande `yq`, qui est disponible pour de nombreuses distributions Linux.

```Bash
# Installation de yq sur Ubuntu
sudo apt-get install yq

# Installation de yq sur CentOS
sudo yum install yq
```

Une fois que vous avez installé `yq`, vous pouvez commencer à manipuler des fichiers YAML en utilisant la commande `yq <commande> <fichier>` dans votre terminal. Par exemple, pour afficher le contenu d'un fichier YAML, vous pouvez utiliser la commande `yq read <fichier>`.

```Bash
# Afficher le contenu d'un fichier YAML
yq read fichier.yml
```

Pour modifier des données dans un fichier YAML, vous pouvez utiliser la commande `yq write <fichier> <chemin> <valeur>` en spécifiant le chemin d'accès à l'élément que vous souhaitez modifier et la valeur à lui assigner.

```Bash
# Modifier une valeur dans un fichier YAML
yq write fichier.yml domaine.domaineTiers.env production
```

Il est également possible d'utiliser la commande `yq delete <fichier> <chemin>` pour supprimer un élément du fichier YAML spécifié.

```Bash
# Supprimer un élément dans un fichier YAML
yq delete fichier.yml domaine.domaineTiers.env
```

## Plongée en profondeur

Maintenant que vous avez une idée de base de l'utilisation de YAML avec Bash, voici quelques informations supplémentaires pour vous aider à mieux comprendre ce langage de sérialisation de données.

- YAML signifie "YAML Ain't Markup Language" et est souvent utilisé pour créer des fichiers de configuration et de données.
- Les données YAML sont formatées sous forme de liste clé-valeur, avec une indentation pour indiquer les relations entre les différentes données.
- `yq` n'est pas seulement limité à l'édition de fichiers YAML, il peut également être utilisé pour convertir des fichiers au format JSON ou pour extraire des données à partir de fichiers XML.

## Voir aussi

Vous pouvez en apprendre plus sur YAML en consultant ces ressources :

- [Documentation officielle de YAML](https://yaml.org/)
- [Documentation officielle de `yq`](https://mikefarah.github.io/yq/)
- [Guide de démarrage rapide de YAML](https://learnxinyminutes.com/docs/fr-fr/yaml-fr/)