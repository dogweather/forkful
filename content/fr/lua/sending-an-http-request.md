---
title:                "Envoi d'une requête HTTP"
date:                  2024-01-20T17:59:55.710254-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Envoyer une requête HTTP, c'est demander des données à un serveur web. On le fait souvent pour intégrer du contenu web dans nos apps ou pour communiquer avec des services en ligne.

## How to: (Comment faire :)

```Lua
-- On utilise le module 'socket.http' pour les requêtes HTTP.
local http = require("socket.http")

-- Envoi d'une requête GET simple.
local reponse, status_code, headers = http.request("http://example.com")

-- Affichage de la réponse et du code de statut
print(reponse)
print(status_code)
```

Sortie attendue :
```
Le contenu HTML de http://example.com
200
```

## Deep Dive (Plongée en profondeur)
Lua n'a pas de fonctionnalités HTTP intégrées. On utilise souvent luasocket pour envoyer des requêtes HTTP. Historiquement, des alternatives comme curl ou des composants externes étaient nécessaires. Le choix de la bibliothèque dépend de vos besoins spécifiques, comme la gestion des cookies ou des appels asynchrones. luasocket est simple pour des requêtes de base, mais pour quelque chose de plus complexe, vous pourriez vouloir regarder vers des bibliothèques comme 'lua-http' ou 'luasec' pour HTTPS.

## See Also (Voir aussi)
- [LuaSocket documentation](http://w3.impa.br/~diego/software/luasocket/http.html)
- [lua-http documentation](https://daurnimator.github.io/lua-http/)
- [luasec documentation](https://github.com/brunoos/luasec/wiki)
