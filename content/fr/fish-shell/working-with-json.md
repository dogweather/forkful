---
title:                "Travailler avec json"
html_title:           "Fish Shell: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des données structurées, telles que des configurations ou des API, il y a de fortes chances que vous rencontriez le format JSON. C'est pourquoi il est important de savoir comment manipuler ce format de données efficacement avec Fish Shell.

## Comment faire

### Installation

Pour travailler avec JSON dans Fish Shell, vous devez d'abord installer le paquet `jq`. Vous pouvez le faire avec la commande suivante :

```
brew install jq
```

ou

```
apt-get install jq
```

selon votre système d'exploitation.

### Lire un fichier JSON

Pour lire les données d'un fichier JSON, vous pouvez utiliser la commande `jq` suivie du chemin du fichier :

```
$ jq '.nom_de_la_clef' chemin/vers/le/fichier.json
```

Par exemple, si vous avez un fichier `config.json` avec le contenu suivant :

```
{
  "prenom": "Marie",
  "age": 35,
  "hobbies": ["lecture", "escalade"]
}
```

Vous pouvez lire la valeur de la clef "age" avec la commande suivante :

```
$ jq '.age' config.json
```

Ce qui retournera `35`.

### Parcourir une liste

Si vous avez une liste de valeurs dans votre fichier JSON, vous pouvez utiliser la fonction `[]` pour accéder à des éléments spécifiques de cette liste. Par exemple, si vous voulez récupérer le premier hobby de la liste "hobbies", vous pouvez utiliser la commande suivante :

```
$ jq '.hobbies[0]' config.json
```

Ce qui retournera `lecture`.

### Filtrer les données

La commande `jq` prend également en charge les filtres pour vous aider à extraire des données spécifiques à partir d'un fichier JSON. Par exemple, si vous ne voulez récupérer que les personnes ayant plus de 30 ans dans votre fichier de config, vous pouvez utiliser cette commande :

```
$ jq 'select(.age > 30)' config.json
```

Ce qui retournera :

```
{
  "prenom": "Marie",
  "age": 35,
  "hobbies": ["lecture", "escalade"]
}
```

## Plongée en profondeur

Il y a tellement plus à apprendre sur l'utilisation de Fish Shell avec JSON. Vous pouvez explorer la documentation officielle de `jq` pour découvrir tous les filtres et fonctions disponibles et les intégrer à vos scripts Fish Shell. Vous pouvez également jeter un coup d'œil à d'autres ressources disponibles en ligne pour vous aider à maîtriser l'utilisation de JSON avec Fish Shell.

## Voir aussi

- [Documentation officielle de `jq`](https://stedolan.github.io/jq/manual/)
- [Tutoriel sur la manipulation de JSON avec Fish Shell](https://medium.com/@joaopsf/simple-guide-to-processing-json-using-fish-shell-42cb821daacc)