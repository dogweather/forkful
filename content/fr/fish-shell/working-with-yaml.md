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

Salut à tous les programmeurs et programmeuses ! Aujourd'hui, nous allons parler de YAML et comment l'utiliser avec le Fish Shell. Si vous êtes curieux(se) de savoir ce qu'est YAML et pourquoi les programmeurs l'utilisent, suivez le guide !

## Qu'est-ce que YAML et pourquoi l'utiliser ?

YAML, ou "YAML Ain't Markup Language", est un format de données qui permet de structurer et stocker des informations de manière lisible et structurée. Cela peut sembler un peu compliqué, mais c'est en fait très utile pour les développeurs car cela leur permet de stocker des configurations ou des données dans un format facilement accessible et modifiable.

## Comment faire ?

Pour utiliser YAML avec le Fish Shell, il vous suffit d'installer le plugin `fish-yaml` en utilisant la commande suivante :

```Fish Shell
omf install fish-yaml
```

Une fois le plugin installé, vous pouvez commencer à travailler avec YAML en utilisant les commandes `yaml get` et `yaml set`, qui vous permettent de récupérer et modifier des données dans un fichier YAML.

Par exemple, si vous avez un fichier YAML nommé `config.yml` avec les données suivantes :

```yaml
fruit:
  - pomme
  - banane
  - kiwi
```

Vous pouvez utiliser la commande `yaml get` pour récupérer le deuxième élément de la liste `fruit` :

```Fish Shell
yaml get config.yml fruit[2]
```

Et vous obtiendrez en sortie le résultat suivant :

```
banane
```

Vous pouvez également utiliser la commande `yaml set` pour modifier les valeurs d'un fichier YAML. Par exemple, si vous voulez ajouter un nouveau fruit à la liste, vous pouvez utiliser cette commande :

```Fish Shell
yaml set config.yml fruit[3] "orange"
```

Et si vous vérifiez le contenu de votre fichier YAML, vous verrez que le nouvel élément a bien été ajouté à la liste.

## Plongée en profondeur

YAML a été créé en 2001 et a été conçu pour être un format de données facilement lisible par les humains. Il est souvent utilisé pour stocker des configurations de logiciels, des données de test ou même du contenu de sites web.

Il existe d'autres formats de données similaires à YAML, tels que JSON et XML, mais YAML se distingue par sa simplicité et sa lisibilité. En plus du Fish Shell, il est également pris en charge par de nombreux autres langages de programmation.

Si vous voulez en savoir plus sur les syntaxes et les fonctionnalités avancées de YAML, vous pouvez consulter la documentation officielle sur leur site web.

## À voir également

Si vous voulez en savoir plus sur les plugins pour le Fish Shell, vous pouvez consulter la [documentation officielle](https://fishshell.com/docs/).

De plus, si vous utilisez YAML pour stocker des configurations, vous pourriez également être intéressé par le plugin de completion `fish-getopt` qui vous permet d'analyser et de traiter facilement des fichiers de configuration en utilisant la convention `getopt`.

Merci d'avoir lu cet article sur l'utilisation de YAML avec le Fish Shell. Nous espérons que cela vous a été utile dans votre travail de développement ! N'hésitez pas à explorer et à découvrir d'autres fonctionnalités intéressantes du Fish Shell. Happy coding !