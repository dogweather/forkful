---
title:                "Le téléchargement d'une page web"
html_title:           "Lua: Le téléchargement d'une page web"
simple_title:         "Le téléchargement d'une page web"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Le téléchargement d'une page web est le fait de récupérer le contenu d'une page web à partir d'Internet. Les programmeurs le font souvent pour extraire des données ou pour automatiser des tâches comme la mise à jour de sites web. 

## Comment faire:
Voici un exemple de code en Lua pour télécharger une page web et afficher son contenu:

```lua
local http = require("socket.http")
local url = "https://www.example.com"
local body, code, _, _ = http.request(url)
if code == 200 then
  print(body)
end
```

Output:
```
<!DOCTYPE html>
<html>
<head>
  <title>Example Domain</title>
 ...
 </body>
</html>
```

## Plongée en profondeur:
Le téléchargement de pages web est un concept très ancien dans le monde de la programmation Internet, remontant aux tout premiers jours du World Wide Web. Bien que Lua n'offre pas de fonction native pour le téléchargement de pages web, il est facile de le faire en utilisant des bibliothèques tierces telles que `socket.http` ou `http.client`. Il existe également des alternatives telles que l'utilisation de l'API de requêtes HTTP en utilisant un serveur tel que Nginx ou Apache. 

## Voir aussi:
- [Socket.http Library](https://w3.impa.br/~diego/software/luasocket/http.html)
- [HTTP Client Library](https://github.com/PedroMiguelSS/Lua_HTTPclient)