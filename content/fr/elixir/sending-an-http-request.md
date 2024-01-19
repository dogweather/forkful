---
title:                "Envoyer une requête http"
html_title:           "Fish Shell: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi ?

Une requête HTTP est une requête standard que votre programme envoie à un serveur pour obtenir ou envoyer des données. Les programmeurs le font pour interagir avec des ressources Web, obtenir des données en direct, etc.

## Comment faire :

Dans Elixir, utiliser `HTTPoison` est un moyen populaire d'envoyer des demandes HTTP. Voici comment vous pouvez le faire :

```elixir
# Assurez-vous d'ajouter httpoison à vos dépendances
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

Pour envoyer une requête GET :

```elixir
defmodule MyHTTP do
  def get(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        IO.puts body
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        IO.puts "Failed with #{status_code}"
      :error ->
        IO.puts "Unknown error"
    end
  end
end

MyHTTP.get("https://api.example.com/data")
```

Si tout est correct, vous verrez la réponse du serveur.

## Approfondissement :

L'envoi d'une requête HTTP est un concept fondamental de la programmation Web. Avant HTTPoison dans Elixir, il y avait d'autres bibliothèques comme `:httpc` du langage Erlang.

Une alternative à HTTPoison est `Tesla`, qui offre un moyen modularisé et flexible d'envoyer des requêtes HTTP. Il y a aussi `Mint`, une autre bibliothèque Elixir, qui prend en charge les connexions HTTP/2.

La mise en œuvre de ces requêtes HTTP dans Elixir implique l'échange de messages au niveau du système d'exploitation sous-jacent pour communiquer avec le serveur Web distant.

## Voir aussi :

- Documentation HTTPoison : https://hexdocs.pm/httpoison
- Tutoriel sur Tesla : https://hexdocs.pm/tesla
- Documentation sur Mint : https://hexdocs.pm/mint