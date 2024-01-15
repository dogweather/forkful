---
title:                "Envoyer une requête http"
html_title:           "Fish Shell: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Pourquoi

Vous vous demandez peut-être pourquoi vous devriez apprendre à envoyer une requête HTTP en utilisant Fish Shell. La réponse est simple : cela peut vous permettre d'interagir avec des API pour récupérer des données, automatiser des tâches, ou encore intégrer votre script Fish dans vos projets web.

# Comment faire

Le processus pour envoyer une requête HTTP en utilisant Fish Shell est relativement simple grâce à la commande intégrée `curl`. Voici un exemple de code et sa sortie correspondante :

```Fish Shell
curl -X GET https://pokeapi.co/api/v2/pokemon/pikachu
```
Sortie :

```
{
	"abilities": [{
		"ability": {
			"name": "static",
			"url": "https://pokeapi.co/api/v2/ability/9/"
		},
		"is_hidden": false,
		"slot": 1
	}, {
		"ability": {
			"name": "lightning-rod",
			"url": "https://pokeapi.co/api/v2/ability/31/"
		},
		"is_hidden": true,
		"slot": 3
	}, {
		"ability": {
			"name": "cute-charm",
			"url": "https://pokeapi.co/api/v2/ability/56/"
		},
		"is_hidden": true,
		"slot": 2
	}],
	"base_experience": 112,
	"forms": [{
		"name": "pikachu",
		"url": "https://pokeapi.co/api/v2/pokemon-form/25/"
	}],
	...
	"types": [{
		"slot": 1,
		"type": {
			"name": "electric",
			"url": "https://pokeapi.co/api/v2/type/13/"
		}
	}],
	"weight": 60
}
```

Comme vous le voyez, la réponse renvoie les informations sur le Pokémon Pikachu depuis l'API PokeAPI.

# Exploration approfondie

La commande `curl` peut être utilisée pour envoyer différents types de requêtes HTTP, tels que GET, POST, PUT et DELETE. Vous pouvez également spécifier des headers, des données à envoyer dans le corps de la requête, et même ajouter des options de sécurité comme des certificats SSL.

Une autre option utile est d'ajouter l'option `-s` pour supprimer la sortie de progression de `curl` et n'afficher que la réponse. De plus, vous pouvez utiliser `jq`, un outil de traitement de JSON en ligne de commande, pour filtrer et manipuler les données de réponse.

Pour en savoir plus sur les différentes options et fonctionnalités de `curl`, vous pouvez consulter la documentation officielle ici : https://curl.haxx.se/docs/. De plus, vous pouvez également explorer les API disponibles et tester vos requêtes en utilisant Postman : https://www.postman.com/.

# Voir aussi

- Documentation officielle de `curl` : https://curl.haxx.se/docs/
- Postman : https://www.postman.com/