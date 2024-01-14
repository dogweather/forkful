---
title:                "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous utilisez un ordinateur ou un téléphone portable, vous utilisez probablement des applications qui envoient des requêtes HTTP. Les requêtes HTTP sont des demandes d'informations à un serveur, qui sont utilisées pour visualiser des sites web, échanger des données ou accéder à des ressources en ligne. Apprendre à envoyer une requête HTTP peut vous aider à mieux comprendre comment fonctionne Internet et à créer vos propres scripts de communication en ligne.

## Comment faire

Afin d'envoyer une requête HTTP en utilisant Bash, vous devez d'abord utiliser la commande `curl`. Voici un exemple basique:

```Bash
curl www.exemple.com
```

Cette commande envoie une requête GET au serveur de www.exemple.com et affiche le contenu de la réponse dans la console. 

Pour spécifier le type de requête, vous pouvez utiliser l'option `-X` et pour ajouter des données au corps de la requête, utilisez l'option `-d`. Voici un exemple de requête POST avec des données JSON:

```Bash
curl -X POST www.exemple.com -d '{"nom": "Marie", "age": 25}'
```

Vous pouvez également spécifier des en-têtes de requête en utilisant l'option `-H`. Voici un exemple avec un en-tête acceptant uniquement les réponses au format JSON:

```Bash
curl -H "Accept:application/json" www.exemple.com
```

En plus de la commande `curl`, il existe également de nombreuses bibliothèques et outils en ligne de commande qui peuvent faciliter l'envoi de requêtes HTTP, tels que `httpie` ou `wget`.

## Plongée en profondeur

Envoyer une requête HTTP implique plusieurs étapes. Tout d'abord, l'ordinateur envoie le message au serveur en utilisant le protocole TCP/IP. Ensuite, le serveur reçoit la requête et peut effectuer des actions en fonction de celle-ci, comme accéder à une ressource ou exécuter un script. Enfin, le serveur envoie une réponse au client, qui peut être un code de statut, un contenu, ou les deux.

Il existe également différentes méthodes de requête HTTP telles que GET, POST, PUT et DELETE, qui ont chacune un objectif différent. Par exemple, GET est utilisé pour récupérer des données, POST pour enregistrer ou créer des données, PUT pour mettre à jour des données existantes et DELETE pour supprimer des données.

Il est également important de comprendre les différents types d'en-têtes de requête que vous pouvez utiliser pour spécifier le type de contenu, l'authentification, etc.

## Voir aussi

- [Introduction à HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Overview)
- [Commande curl](https://curl.haxx.se/docs/manpage.html)
- [outil en ligne de commande HTTPie](https://httpie.org/)
- [bibliothèque de requêtes HTTP en Python](https://requests.readthedocs.io/en/master/)