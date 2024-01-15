---
title:                "Envoi d'une demande http"
html_title:           "Gleam: Envoi d'une demande http"
simple_title:         "Envoi d'une demande http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez dans le développement web, il est à peu près certain que vous allez devoir interagir avec des API et donc, envoyer des requêtes HTTP. Découvrez comment le faire avec Gleam !

## Comment faire

Envoyer une requête HTTP en utilisant Gleam est simple. Tout d'abord, vous devez avoir le module HTTP dans votre projet :

```Gleam
let http = import http
```

Ensuite, vous pouvez utiliser la fonction `request` pour spécifier la méthode, l'URL, les en-têtes et le corps de la requête :

```Gleam
let { Ok, Err } =
  http.request(
    "GET",
    "https://example.com/users",
    [("Content-Type", "application/json")],
    "{
      \"name\": \"John Doe\",
      \"email\": \"john.doe@example.com\"
    }"
  )
```

Notez que cette fonction renvoie un résultat `Ok` ou `Err` en fonction de la réponse de serveur.

Vous pouvez également spécifier des paramètres de requête avec la fonction `params` :

```Gleam
http.request(
  "GET",
  "https://example.com/users",
  [("Content-Type", "application/json")],
  "{
    \"name\": \"John Doe\",
    \"email\": \"john.doe@example.com\"
  }",
  http.params([("filter", "online"), ("sort", "name")])
)
```

Cela ajoutera les paramètres "filter=online" et "sort=name" à l'URL de la requête.

## Plongée en profondeur

Il existe de nombreux autres paramètres et options disponibles pour personnaliser vos requêtes HTTP en utilisant le module HTTP de Gleam. Vous pouvez également utiliser les fonctions `get`, `post`, `put`, `delete` pour raccourcir votre code si vous n'avez pas besoin de spécifier tous les détails de la requête.

## Voir aussi

- La documentation du module HTTP de Gleam : https://gleam.run/modules/http.html
- Une présentation détaillée sur l'envoi de requêtes HTTP en utilisant Gleam : https://www.jameswest.dev/http-requests-gleam/