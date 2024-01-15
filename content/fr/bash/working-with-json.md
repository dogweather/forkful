---
title:                "Travailler avec json"
html_title:           "Bash: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

Travailler avec JSON peut sembler intimidant au premier abord, mais c'est en fait un format très utile et couramment utilisé pour échanger des données entre différentes applications, plateformes et langages de programmation. Il est important de savoir comment travailler avec JSON si vous développez des applications web ou des scripts de manipulation de données.

## Comment Faire

Pour travailler avec JSON en Bash, nous utiliserons l'utilitaire `jq`, qui est un puissant outil de traitement de données JSON en ligne de commande. Tout d'abord, vous devez installer `jq` sur votre système. Si vous utilisez un système basé sur Debian, vous pouvez exécuter la commande suivante pour l'installer:

```Bash
sudo apt-get install jq
```
Si vous utilisez un autre système, vous pouvez trouver des instructions d'installation sur le site officiel de `jq`.

Une fois que `jq` est installé, vous pouvez commencer à travailler avec JSON. Supposons que vous avez un fichier `data.json` avec le contenu suivant:

```Bash
{
  "pays": [
    {
      "nom": "France",
      "capitale": "Paris",
      "population": 67022000
    },
    {
      "nom": "Allemagne",
      "capitale": "Berlin",
      "population": 83019200
    },
    {
      "nom": "Espagne",
      "capitale": "Madrid",
      "population": 47096600
    }
  ]
}
```

Pour lire le contenu de ce fichier JSON, vous pouvez utiliser la commande suivante:

```Bash
jq '.' data.json
```

Cela affichera le contenu formaté du fichier `data.json`, ce qui peut être utile pour la lecture, mais pas pour le traitement des données. Si vous voulez extraire les données de chaque pays, vous pouvez utiliser la commande suivante:

```Bash
jq '.pays[]' data.json
```

Cela affichera une liste contenant les données de chaque pays sous forme d'objets JSON:

```Bash
{
  "nom": "France",
  "capitale": "Paris",
  "population": 67022000         
}
{
  "nom": "Allemagne",
  "capitale": "Berlin",
  "population": 83019200
}
{
  "nom": "Espagne",
  "capitale": "Madrid",
  "population": 47096600
}
```

Vous pouvez également extraire une valeur spécifique en utilisant la notation en point pour accéder à une clé spécifique. Par exemple, si vous voulez extraire la population de chaque pays, vous pouvez utiliser la commande suivante:

```Bash
jq '.pays[].population' data.json
```

Cela affichera simplement la population de chaque pays:

```Bash
67022000
83019200
47096600
```

Il existe de nombreuses autres fonctionnalités et possibilités avec `jq`, mais cela devrait vous donner une idée de base de comment travailler avec JSON en Bash.

## Plongée en Profondeur

Si vous voulez aller plus en profondeur dans l'utilisation de `jq`, vous pouvez consulter la documentation officielle qui est très complète et bien écrite. Vous pouvez également trouver plusieurs tutoriels et exemples en ligne pour vous aider à mieux comprendre comment utiliser `jq`.

Une autre chose intéressante à noter est que `jq` peut également être utilisé pour manipuler les données dans des API REST en utilisant la commande `curl`. Cela peut être très utile pour automatiser certaines tâches dans vos scripts Bash.

Enfin, gardez à l'esprit que `jq` est un outil en constante évolution, avec de nouvelles fonctionnalités ajoutées régulièrement. N'hésitez pas à explorer et à expérimenter pour en apprendre davantage sur ce que vous pouvez faire avec `jq` en Bash.

## À Voir

- [Site officiel `jq`](https://stedolan.github.io/jq/)
- [Documentation `jq`](https://stedolan.github.io/jq/manual/)
- [Tutoriel `jq` par Exercism](https://exercism.io/tracks/bash/exercises/json-parser/solutions/c36bd1ebe0dc46d89ffaf06a2eb36384)
- [Exemples de manipulation de données avec `jq` par DigitalOcean](https://www.digitalocean.com/community/tutorials/how-to-use-jq-to-manage-json-from