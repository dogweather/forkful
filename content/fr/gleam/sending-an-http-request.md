---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Envoyer une requête HTTP, c'est demander l'accès à des informations spécifiques sur le Web. Les programmeurs en ont besoin pour accéder à des données externes pour leur application.

## Comment faire :
Voici un rapide exemple sur comment envoyer une requête HTTP en Gleam:
```gleam
let request = gleam/httpc/request(ini_url("http://monsite.com")?)
  |> gleam/httpc/to_service("localhost", 8000)
req
|> gleam/httpc.send()
|> tuple.either
```

Votre sortie ressemblera probablement à cela :
```gleam
Ok(#Response{
  status: 200,
  headers: [header("Content-Type", "text/plain")],
  body: "Bonjour, Monde!"
})
```

## Approfondissement
Historiquement, l'envoi de requêtes HTTP est une des tâches les plus importantes en programmation web. Des alternatives comme les requêtes WebSocket existent, permettant une communication bidirectionnelle, mais les requêtes HTTP restent la norme pour l'accès aux données.

En Gleam, la `gleam/httpc` est utilisée pour configurer et envoyer les requêtes. L'opération `gleam/httpc.send()` est celle qui fait le plus gros du travail. Elle prend la requête HTTP et la fournit à un service sur un hôte spécifique via une connexion réseau.

## Pour en savoir plus
Jetez un œil aux ressources suivantes pour en savoir plus sur l'envoi de requêtes HTTP avec Gleam:
- Documentation officielle de `gleam/httpc`: https://hexdocs.pm/gleam_stdlib/gleam/httpc/
- Guide de l'utilisateur de Gleam: https://gleam.run/book/
- Stack Overflow - Gleam et les requêtes HTTP: https://stackoverflow.com/questions/tagged/gleam