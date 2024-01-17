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

## Quoi & Pourquoi?
Le fait d'envoyer une requête HTTP consiste à demander à un serveur web des informations spécifiques via un protocole de communication. Les programmeurs le font pour récupérer des données ou interagir avec des applications en ligne.

## Comment faire:
Voici un exemple de code en Elixir pour envoyer une requête HTTP:
```Elixir
query_string = "param1=value1&param2=value2"
uri = URI.parse("https://example.com/path?#{query_string}")
{:ok, response} = HTTPoison.get!(uri)
```

## Plongez en profondeur:
L'envoi de requêtes HTTP est une pratique fondamentale dans le développement web et a été introduit dans les années 1990. Des alternatives telles que REST, SOAP et GraphQL ont été développées pour améliorer les performances et la flexibilité. Dans Elixir, la bibliothèque HTTPoison est largement utilisée pour effectuer des requêtes HTTP.

## Voir aussi:
- [Documentation officielle de HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Article sur les différents types de requêtes HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Messages)