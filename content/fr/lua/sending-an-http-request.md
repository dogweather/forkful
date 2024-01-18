---
title:                "Envoi d'une requête http"
html_title:           "Lua: Envoi d'une requête http"
simple_title:         "Envoi d'une requête http"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi? 
Envoyer une requête HTTP est simplement le fait d'envoyer une demande ou une question à un serveur web. Les programmeurs le font souvent pour récupérer des données depuis un site web ou pour effectuer une action sur celui-ci.

# Comment faire: 
Pour envoyer une requête HTTP en Lua, nous utiliserons la bibliothèque LuaSocket. Voici un exemple de code qui envoie une requête GET à l'URL "www.example.com" et affiche le code de réponse et la réponse elle-même:

```Lua
local socket = require("socket.http")
local response, code = socket.request("http://www.example.com")
print(code)
print(response)
```

La sortie sera quelque chose comme:

```
200
<!DOCTYPE html>
<html>
<head>
... (rest of the HTML code)
```

# Plongeons plus en profondeur: 
Avant de pouvoir envoyer une requête HTTP, il faut comprendre le fonctionnement de ce protocole de communication. Il a été créé dans les années 90 pour permettre aux utilisateurs d'intéragir avec les ressources en ligne. Il existe d'autres alternatives telles que FTP ou SMTP pour communiquer avec des serveurs, mais HTTP est le plus couramment utilisé pour les sites web. En implémentant une requête HTTP, il faut également prendre en compte des détails tels que les en-têtes et les types de contenu.

# Voir aussi: 
Pour en savoir plus sur l'envoi de requêtes HTTP en Lua, voici quelques liens utiles:

- La documentation officielle de la bibliothèque LuaSocket: http://w3.impa.br/~diego/software/luasocket/http.html
- Un tutoriel détaillé sur l'envoi et la réception de requêtes HTTP en Lua: https://coronalabs.com/blog/2013/04/16/networking-with-lua/
- Le protocole HTTP sur le site de l'Internet Engineering Task Force (IETF): https://www.ietf.org/rfc/rfc2616.txt