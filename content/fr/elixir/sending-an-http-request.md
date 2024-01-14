---
title:                "Elixir: Envoyer une demande http"
simple_title:         "Envoyer une demande http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Envoyer une requête HTTP est une tâche courante en programmation web. Cela permet à une application d'interagir avec un serveur distant et d'obtenir des données précises. Dans cet article, nous allons explorer comment cela fonctionne en utilisant le langage de programmation Elixir.

## Comment Faire

Pour effectuer une requête HTTP en utilisant Elixir, nous allons utiliser la librairie HTTPoison. Tout d'abord, nous devons l'ajouter à notre projet en ajoutant `{:httpoison, "~> 1.0"}` à notre fichier Mix.exs. Ensuite, nous pouvons l'installer en exécutant `mix deps.get` dans notre terminal.

Une fois que nous avons installé HTTPoison, nous pouvons l'utiliser pour envoyer une requête GET à un serveur en utilisant la fonction `HTTPoison.get/2`. Par exemple, si nous voulons accéder à la page d'accueil de Google, nous pouvons utiliser le code suivant :

```Elixir
response = HTTPoison.get("https://www.google.com")
```

Cela enverra une requête GET à l'URL spécifiée et stockera la réponse dans la variable `response`. Nous pouvons ensuite accéder aux données de réponse en utilisant la syntaxe `response.body` pour obtenir le corps de la réponse et `response.status_code` pour obtenir le code de statut de la réponse.

## Plongée Profonde

Maintenant que nous avons vu comment envoyer une requête HTTP basique, il est important de comprendre certains concepts et options clés lors de l'envoi de requêtes.

#### Méthodes HTTP
Il existe différentes méthodes pour envoyer une requête HTTP, telles que GET, POST, PUT, DELETE, etc. Pour spécifier une méthode autre que GET, nous pouvons utiliser le paramètre `method` lors de l'appel à `HTTPoison.get/2`. Par exemple, pour envoyer une requête POST, nous pouvons utiliser `HTTPoison.get("https://www.example.com", [method: :post])`.

#### Envoi de données
Dans certains cas, nous devons également envoyer des données en plus de notre requête. Pour cela, nous pouvons utiliser le paramètre `body` en passant une carte ou une chaîne de caractères en tant que données. Par exemple, pour envoyer une requête POST avec des données JSON, nous pouvons utiliser `HTTPoison.get("https://www.example.com", [method: :post, body: %{key: "value"}])`.

## Voir Aussi

- [ Documentation HTTPoison ](https://hexdocs.pm/httpoison/1.0.0/HTTPoison.html)
- [ Guide de Référence HTTP ] (https://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html)