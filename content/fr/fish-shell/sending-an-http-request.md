---
title:                "Fish Shell: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi envoyer une requête HTTP?

L'envoi de requêtes HTTP est une pratique courante pour interagir avec des serveurs web. Que ce soit pour obtenir des informations, envoyer des données ou effectuer des actions, cette méthode est essentielle pour communiquer avec des services en ligne.

## Comment faire

Pour envoyer une requête HTTP avec Fish Shell, utilisez la commande `curl`. Voici un exemple de requête GET simple :

```Fish Shell
curl www.example.com 
```

Vous pouvez également spécifier des méthodes différentes (POST, PUT, DELETE) et ajouter des paramètres ou des en-têtes :

```Fish Shell
curl -X POST -H "Content-Type: application/json" -d '{"nom": "John", "age": 25}' www.example.com/api/users
```

## Plongée en profondeur

Il est possible d'effectuer des requêtes plus avancées en utilisant le module `httpie` dans Fish Shell. Ce module offre une syntaxe plus simple et intuitive pour envoyer des requêtes HTTP, avec une mise en forme plus claire des résultats :

```Fish Shell
http POST www.example.com/api/users nom="John" age:=25
```

Il est également important de connaître les codes de statut HTTP pour comprendre la réponse reçue du serveur. Les codes les plus courants sont 200 (OK), 404 (Non trouvé) et 500 (Erreur interne du serveur).

## Voir aussi

- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guide de référence sur les requêtes HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Overview)
- [Tutoriel Fish Shell](https://www.howtoforge.com/tutorial/fish-shell-beginners-guide/)