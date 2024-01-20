---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Envoie d'une requête HTTP avec authentification de base, ceci signifie coder votre identifiant et mot de passe en base64 et l'envoyer en tant que partie de votre requête HTTP. Les développeurs le font souvent pour interagir avec des API qui exigent cette forme d'authentification.

## Comment Faire :

Utiliser HTTPoison pour envoyer une requête GET. HTTPoison est une bibliothèque Elixir simple et rapide pour envoyer des requêtes HTTP.

```elixir
defmodule BasicAuth do
  @moduledoc """
  Module for handling Basic Auth in HTTP requests.
  """

  def send_request(url, username, password) do
    headers = basic_auth_header(username, password)
    HTTPoison.get(url, headers)
  end

  defp basic_auth_header(username, password) do
    auth = "#{username}:#{password}"
    basic_auth = Base.encode64(auth)
    [{"Authorization", "Basic #{basic_auth}"}]
  end
end
```

Exemple de sortie:

```elixir
{:ok, %HTTPoison.Response{status_code: 200, body: response_body}}
```

## Plongée Profonde :

Historiquement, l'authentification de base a été utilisée pour la première fois en 1994 dans le protocole HTTP. C'est une méthode couramment utilisée, mais pas la plus sécurisée en raison de son manque de chiffrement. 

En alternative à l'authentification de base, on peut utiliser l'authentification par le porte-jeton ou `Bearer Token Authentication`. Elle est généralement utilisée pour les API REST car elle est plus simple et plus sécurisée.

Le détail de mise en œuvre dans Elixir inclut l'utilisation de `HTTPoison`, qui est basé sur la bibliothèque Erlang `hackney`. Il vous suffit d'encoder l'identifiant et le mot de passe en utilisant `Base.encode64` et les inclure dans le header de la requête HTTP.

## Voir Aussi :

1. La documentation officielle de HTTPoison : [https://hexdocs.pm/httpoison/readme.html](https://hexdocs.pm/httpoison/readme.html)
2. Plus de détails sur l'authentification de base : [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication)
3. Guide de l'authentification Référentiel : [https://jwt.io/introduction/](https://jwt.io/introduction/)