---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Envoyer une requête HTTP, c'est comme envoyer une lettre demandant des données à un serveur web. Les programmeurs le font pour interagir avec les API web, récupérer des informations, soumettre des données, entre autres.

## Comment faire:

Voici un exemple simple pour envoyer une requête GET en utilisant la bibliothèque Lua `http.request`:

```Lua
local http = require("socket.http")
local body, code = http.request("http://example.com")
if code == 200 then
    print(body)
else
    print("Erreur: ".. code)
end
```
Dans cet exemple, une requête est envoyée à `http://example.com`. Si la requête réussit (code de statut HTTP 200), le contenu de la page est affiché. En cas d'erreur, le code d'erreur est affiché.

## Plongée en profondeur:

Historiquement, envoyer des requêtes HTTP en Lua nécessitait l'utilisation de bibliothèques externes car le 'core' Lua n'inclut pas ce support. De nos jours, de nombreuses bibliothèques permettent cela, comme `lua-http` ou `luasocket`.

Il existe également d'autres méthodes pour envoyer des requêtes HTTP, par exemple, avec `luasec` pour des requêtes HTTPS.

Quant à l'implémentation, dans les coulisses, une requête HTTP est envoyée au serveur en utilisant le protocole TCP. Le serveur répond avec un message de statut (par exemple, 200 pour OK) et les données demandées.

## Voir aussi:

* Documentation LuaSocket : http://w3.impa.br/~diego/software/luasocket/http.html
* Bibliothèque lua-http : https://github.com/daurnimator/lua-http
* Documentation luasec: https://github.com/brunoos/luasec/wiki