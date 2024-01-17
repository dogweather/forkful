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

## Qu'est-ce que c'est et pourquoi?

Travailler avec JSON peut sembler intimidant pour certains programmeurs, mais en réalité, c'est simplement un format de données utilisé pour stocker et échanger des informations. Il est utilisé pour structurer des données de manière à les rendre facilement lisibles pour les machines et les humains. Les programmeurs utilisent JSON pour stocker des données dans des applications, créer des API et échanger des informations avec des serveurs.

## Comment faire:

Il y a quelques commandes utiles en Fish Shell pour travailler avec des données JSON. L'une d'entre elles est `jq`, qui est un outil de ligne de commande qui peut être utilisé pour traiter et filtrer des données JSON. Par exemple, si nous avons un fichier `data.json` avec une liste de noms et d'âges, nous pouvons utiliser la commande suivante pour afficher uniquement les noms:

```
jq '.noms' data.json
```

Nous pouvons également utiliser la commande `curl` pour effectuer des requêtes HTTP et récupérer des données JSON à partir d'une API. Par exemple, pour récupérer des informations météorologiques à partir de l'API OpenWeather, nous pouvons utiliser la commande suivante:

```
curl 'http://api.openweathermap.org/data/2.5/weather?q=Paris&appid=your_api_key'
```

Voici un exemple de sortie JSON que nous pourrions obtenir en utilisant la commande ci-dessus:

```
{
  "coord": {
    "lon": 2.35,
    "lat": 48.85
  },
  "weather": [
    {
      "id": 800,
      "main": "Clear",
      "description": "clear sky",
      "icon": "01d"
    }
  ],
  "base": "stations",
  "main": {
    "temp": 22.97,
    "feels_like": 21.11,
    "temp_min": 21.67,
    "temp_max": 24.44,
    "pressure": 1014,
    "humidity": 47
  },
  "visibility": 10000,
  "wind": {
    "speed": 3.6,
    "deg": 340
  },
  "clouds": {
    "all": 0
  },
  "dt": 1560739216,
  "sys": {
    "type": 1,
    "id": 6540,
    "message": 0.0091,
    "country": "FR",
    "sunrise": 1560692003,
    "sunset": 1560749664
  },
  "timezone": 7200,
  "id": 2988507,
  "name": "Paris",
  "cod": 200
}
```

## Profondeur:

JSON a été créé en 2001 par Douglas Crockford et est devenu un format de données très populaire en raison de sa simplicité et de sa lisibilité. Bien qu'il soit souvent utilisé dans les applications web, il existe d'autres alternatives telles que XML ou YAML pour stocker et échanger des données. Sur Fish Shell, la plupart des commandes qui peuvent traiter des données JSON utilisent le format `dot notation` pour accéder aux données.

## Voir aussi:

Pour en savoir plus sur comment travailler avec JSON en Fish Shell, vous pouvez consulter la documentation officielle de Fish Shell ainsi que les sources suivantes:

- https://fishshell.com/docs/current/
- https://www.json.org/
- https://stedolan.github.io/jq/