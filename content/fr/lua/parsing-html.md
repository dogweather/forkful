---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:32:37.205490-07:00
html_title:           "Arduino: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"

category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? / Quoi et Pourquoi ?
L'analyse (parsing) HTML consiste à interpréter le code HTML pour en extraire des données. Les programmeurs font cela pour manipuler, nettoyer ou récupérer des informations depuis des pages web.

## How to / Comment faire :
Voici un exemple simple en Lua pour parser du HTML en utilisant la bibliothèque `lxp`. On va extraire le titre d'une page HTML.

```Lua
local lxp = require 'lxp'

local contents = [[
<html>
<head>
<title>Exemple de Page</title>
</head>
<body>
<h1>Bienvenue!</h1>
</body>
</html>
]]

local title
local capture = false

callbacks = {
  StartElement = function (parser, tagname)
    if tagname == "title" then capture = true end
  end,
  CharacterData = function (parser, string)
    if capture then title = string end
  end,
  EndElement = function (parser, tagname)
    if tagname == "title" then capture = false end
  end,
}

local p = lxp.new(callbacks)
p:parse(contents)
p:parse() -- fin du document
p:close()

print("Le titre trouvé est :", title)
```

Sortie :

```
Le titre trouvé est : Exemple de Page
```

## Deep Dive / Exploration Approfondie :
Le parsing HTML est fondamental depuis la naissance du web. Historiquement, des bibliothèques robustes comme `BeautifulSoup` pour Python dominent. En Lua, `lxp` (basée sur la bibliothèque Expat XML) est fréquemment utilisée, bien qu'elle nécessite que le HTML soit bien formé. Les autres options en Lua incluent `htmlparser` et `gumbo`, qui sont plus indulgentes avec le HTML mal formé.

Un détail important lors du parsing HTML est de gérer correctement l'encodage des caractères et les entités HTML. Utiliser une bibliothèque dédiée permet d'éviter de nombreux pièges, comme les erreurs de syntaxe ou la perte de contenu important.

## See Also / Voir Aussi :
- Lua Expat (lxp) : [https://github.com/lunarmodules/luaexpat](https://github.com/lunarmodules/luaexpat)
- Htmlparser Lua : [https://github.com/msva/lua-htmlparser](https://github.com/msva/lua-htmlparser)
- Gumbo parser : [https://github.com/craigbarnes/lua-gumbo](https://github.com/craigbarnes/lua-gumbo)
