---
date: 2024-01-20 17:59:55.710254-07:00
description: "Envoyer une requ\xEAte HTTP, c'est demander des donn\xE9es \xE0 un serveur\
  \ web. On le fait souvent pour int\xE9grer du contenu web dans nos apps ou pour\
  \ communiquer\u2026"
lastmod: 2024-02-19 22:05:16.655076
model: gpt-4-1106-preview
summary: "Envoyer une requ\xEAte HTTP, c'est demander des donn\xE9es \xE0 un serveur\
  \ web. On le fait souvent pour int\xE9grer du contenu web dans nos apps ou pour\
  \ communiquer\u2026"
title: "Envoi d'une requ\xEAte HTTP"
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
