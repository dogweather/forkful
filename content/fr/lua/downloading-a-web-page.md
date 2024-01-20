---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ceet Pourquoi?

Télécharger une page web est l'action de copier les données d'une page web d'un serveur à votre machine locale. Les programmeurs le font pour analyser ou manipuler les données, construire des applications intéressantes ou résoudre des problèmes spécifiques.

## Comment faire :

Voici un simple script Lua utilisant par exemple le module `luasocket.http`.

```Lua
local http = require("socket.http")

local body, code, headers, status = http.request("http://example.com")

if code == 200 then 
    print(body)
else
    print(status)
end
```

Si tout se passe bien, cela imprimera le contenu de `example.com`. Si quelque chose va mal, il imprimera le statut d'erreur.

## Plongeons plus profondément :

Historiquement, les données sur le Web étaient récupérées à l'aide de diverses bibliothèques et outils, mais avec l'évolution de la technologie Internet, des outils plus simples et plus efficaces ont été développés pour effectuer la tâche. LuaSocket est l'un de ces outils, une bibliothèque de programmation réseau pour Lua, et c'est pourquoi il est souvent utilisé pour ce genre de tâches.

Les alternatives à LuaSocket pour télécharger une page web pourraient être des bibliothèques telles que `luajit-request` ou `lua-http`, qui fournissent également un moyen pratique de faire des requêtes HTTP.

Le processus pour télécharger une page web en Lua est assez simple. Lorsque vous exécutez votre script, Lua envoie une requête GET HTTP au serveur web. Le serveur web répond ensuite avec le code HTML de la page, que vous pouvez ensuite traiter ou analyser en Lua.

## À voir également :

- LuaSocket : http://w3.impa.br/~diego/software/luasocket/http.html
- Luajit-request : https://github.com/LPGhatguy/luajit-request
- Lua-http : https://github.com/daurnimator/lua-http