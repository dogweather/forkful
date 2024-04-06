---
date: 2024-01-20 18:01:29.417273-07:00
description: "Comment faire : L'authentification de base HTTP existe depuis les d\xE9\
  buts du web, comme moyen simple mais moins s\xE9curis\xE9 d'acc\xE9der aux services\
  \ en ligne,\u2026"
lastmod: '2024-04-05T21:53:58.906355-06:00'
model: gpt-4-1106-preview
summary: "L'authentification de base HTTP existe depuis les d\xE9buts du web, comme\
  \ moyen simple mais moins s\xE9curis\xE9 d'acc\xE9der aux services en ligne, car\
  \ les identifiants sont encod\xE9s mais non chiffr\xE9s."
title: "Envoi d'une requ\xEAte HTTP avec authentification de base"
weight: 45
---

## Comment faire :
```elixir
# Ajout de la dépendance HTTPotion dans mix.exs
defp deps do
  [
    {:httpotion, "~> 3.1.0"}
  ]
end

# Exemple simple de requête avec authentification de base
def send_request_with_basic_auth do
  # Encode the credentials
  basic_auth = Base.encode64("user:password")

  # Set the headers
  headers = ["Authorization": "Basic #{basic_auth}"]

  # Make the request
  response = HTTPotion.get("https://your-api-endpoint.com/resource", headers: headers)

  # Output the response (sample output placeholder)
  IO.inspect(response.body)
end
```

## Exploration approfondie
L'authentification de base HTTP existe depuis les débuts du web, comme moyen simple mais moins sécurisé d'accéder aux services en ligne, car les identifiants sont encodés mais non chiffrés. En alternative, on utilise souvent l'authentification par jetons (tokens) comme bearer tokens dans les headers de requêtes, ou mieux encore, OAuth2 pour des schémas d'authentification plus complexes et sécurisés. Techniquement, lorsqu'on envoie une requête avec authentification de base en Elixir, le package choisi (ici HTTPotion) s'occupe des basses œuvres : il encode les identifiants et prépare les headers HTTP appropriés.

## Voir également
- [HTTPotion GitHub repository](https://github.com/myfreeweb/httpotion)
- [Elixir Base Module Documentation](https://hexdocs.pm/elixir/Base.html)
- [HTTP basic access authentication on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Introduction to HTTP Authentication Schemes](https://developer.okta.com/blog/2019/06/04/what-the-heck-is-sign-in-with-apple)
