---
title:                "Envoi d'une requête http"
html_title:           "Elixir: Envoi d'une requête http"
simple_title:         "Envoi d'une requête http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous souhaitez connecter votre application à un serveur web ou à un API, vous aurez probablement besoin d'envoyer une requête HTTP. Cela permet de communiquer avec le serveur et d'échanger des données. L'utilisation de requêtes HTTP est un élément clé dans le développement d'applications web modernes.

## Comment Faire

Pour envoyer une requête HTTP en utilisant Elixir, vous pouvez utiliser le module `HTTPoison`. Tout d'abord, vous devez installer le paquet en l'ajoutant à votre `mix.exs` et en exécutant `mix deps.get`. Ensuite, vous pouvez importer le module `HTTPoison` dans votre fichier `ex` :

```Elixir
import HTTPoison
```

Ensuite, vous pouvez utiliser la fonction `get` pour envoyer une requête `GET` à une URL spécifique :

```Elixir
response = get("https://www.example.com")
```

Vous pouvez également ajouter des paramètres à votre requête en utilisant un tuple de clé/valeur :

```Elixir
response = get("https://www.example.com/search", {q: "elixir"})
```

Si vous souhaitez envoyer une requête `POST` avec un corps de requête, vous pouvez utiliser la fonction `post` et passer un hash avec le contenu du corps :

```Elixir
body = %{username: "John", password: "secret"}
response = post("https://www.example.com/login", body)
```

Une fois la requête envoyée, vous pouvez accéder au corps de la réponse en utilisant `response.body`. Vous pouvez également vérifier le code de statut de la réponse en utilisant `response.status`.

## Plongée en profondeur

HTTPoison offre des fonctionnalités avancées telles que la possibilité d'envoyer des requêtes avec des en-têtes personnalisées, de gérer les redirections et les cookies. Vous pouvez également utiliser le module `HTTPoison.Request` pour créer des requêtes personnalisées avec des options supplémentaires.

Il est important de comprendre les différents types de requêtes HTTP telles que `GET`, `POST`, `PUT`, `DELETE` et `PATCH` et comment ils sont utilisés pour interagir avec les serveurs. Vous devez également être conscient des codes de statut de réponse courants tels que 200, 404 et 500 et comment les gérer dans votre code.

## Voir aussi

- [Documentation HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Guide Elixir pour les requêtes HTTP](https://elixirschool.com/fr/lessons/advanced/http/)
- [Les codes de statut HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Status)