---
date: 2024-01-20 17:44:39.885188-07:00
description: "How to: (Comment faire :) Sortie \xE9chantillon ."
lastmod: '2024-04-05T21:53:59.408425-06:00'
model: gpt-4-1106-preview
summary: "(Comment faire :) Sortie \xE9chantillon ."
title: "T\xE9l\xE9chargement d'une page web"
weight: 42
---

## How to: (Comment faire :)
```Lua
-- Vous aurez besoin de 'luasocket'
local http = require("socket.http")

-- Téléchargez le contenu de la page et stockez-le dans une variable 'body'
local body, status, headers = http.request("http://www.example.com")

-- Vérifiez si la requête a réussi
if status == 200 then
    print("Page téléchargée avec succès !")
    print(body)
else
    print("Erreur lors du téléchargement : ", status)
end
```
Sortie échantillon :
```
Page téléchargée avec succès !
[Contenu de la page web ici...]
```

## Deep Dive (Plongée en profondeur)
Historiquement, Lua n'a pas été conçu avec le téléchargement de pages web en tête. Cependant, la communauté a développé 'luasocket', une bibliothèque tierce, pour combler ce manque. Autres options ? 'wget' ou 'curl' peuvent être utilisés via `os.execute()`, mais c'est moins élégant. Concernant l'implémentation, Lua facilite les choses via les coroutines pour gérer l'asynchronisme potentiel de ces opérations de réseau.

## See Also (Voir Aussi)
- La documentation de 'luasocket' : [http://w3.impa.br/~diego/software/luasocket/](http://w3.impa.br/~diego/software/luasocket/)
- Documentation de Lua : [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
