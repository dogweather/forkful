---
title:                "Envoi d'une requête HTTP"
aliases:
- fr/elixir/sending-an-http-request.md
date:                  2024-01-20T17:59:18.647136-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Envoyer une requête HTTP permet à votre application de communiquer avec le monde extérieur : récupérer des données, interagir avec des API, etc. Les programmeurs utilisent ça pour accéder à des ressources web et pour intégrer des fonctionnalités externe.

## How to: (Comment faire : )
Pour envoyer une requête HTTP en Elixir, on utilise souvent la bibliothèque HTTPoison. Assurez-vous de l'avoir ajoutée à votre `mix.exs`.

```elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

Puis, lancez ces commandes dans votre shell :

```shell
mix deps.get
mix deps.compile
```

Maintenant, envoyez une requête GET simple :

```elixir
response = HTTPoison.get!("https://jsonplaceholder.typicode.com/posts/1")
IO.inspect response
```

Et voilà un extrait possible de la réponse :

```elixir
%HTTPoison.Response{
  body: "{ \"userId\": 1, \"id\": 1, \"title\": \"Post title ...",
  status_code: 200,
  ...
}
```

## Deep Dive (Plongée en profondeur)
Historiquement, Elixir utilisait la bibliothèque HTTP standard `:httpc`. `HTTPoison` est une enveloppe autour de `hackney`, une autre bibliothèque Erlang, et est préféré pour sa simplicité d'utilisation et sa documentation claire.

Vous avez des alternatives comme `Tesla` ou `Finch`, toutes deux avec des approches et des configurations différentes. Choisir entre elles dépend de vos besoins spécifiques en termes de performance, de middleware ou d'adaptabilité.

Au niveau de l'implémentation, lorsque vous envoyez une requête, un processus sous-jacent ouvre une connexion TCP/SSL vers le serveur cible. Les données sont ensuite sérialisées au format HTTP, envoyées, et on attend une réponse qui sera également désérialisée.

## See Also (Voir aussi)
- [HTTPoison GitHub](https://github.com/edgurgel/httpoison)
- [Finch GitHub](https://github.com/sneako/finch)
- [Tesla GitHub](https://github.com/teamon/tesla)
- [jsonplaceholder.typicode.com](https://jsonplaceholder.typicode.com/) - pour tester des requêtes HTTP avec de faux endpoints.
