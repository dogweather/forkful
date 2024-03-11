---
date: 2024-01-26 04:33:30.133890-07:00
description: "Travailler avec XML implique l'analyse et la manipulation de documents\
  \ XML \xE0 l'aide de code. Les programmeurs font cela pour lire, \xE9crire et modifier\
  \ des\u2026"
lastmod: '2024-03-11T00:14:31.902567-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec XML implique l'analyse et la manipulation de documents XML\
  \ \xE0 l'aide de code. Les programmeurs font cela pour lire, \xE9crire et modifier\
  \ des\u2026"
title: Travailler avec XML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Travailler avec XML implique l'analyse et la manipulation de documents XML à l'aide de code. Les programmeurs font cela pour lire, écrire et modifier des données dans un format structuré et portable, largement utilisé pour l'échange et le stockage de données.

## Comment faire :
Lua n'inclut pas d'analyse XML native, mais il existe des bibliothèques comme LuaXML et xml2lua qui font le travail. Voici un bref aperçu de l'analyse XML avec xml2lua :

```Lua
local xml2lua = require("xml2lua")
local handler = require("xmlhandler.tree")

local xmlParser = xml2lua.parser(handler)
xmlParser:parse([[<root><book id="123">Programmation en Lua</book></root>]])

print(handler.root.book._attr.id)  -- Affiche : 123
print(handler.root.book[1])        -- Affiche : Programmation en Lua
```

Pour écrire du XML, voici un mini exemple utilisant LuaXML :

```Lua
local luaxml = require("LuaXML")

local xml = xml.new("root")
xml:append("book")[1] = "Programmation en Lua"
xml.book._attr = {id="123"}

print(xml:tag())  -- Affiche : <root><book id="123">Programmation en Lua</book></root>
```

## Plongée profonde
XML, abréviation de Extensible Markup Language, est un standard dans la représentation et l'échange de données depuis le milieu des années 90. Il donne une structure aux données et est à la fois lisible par l'homme et analysable par machine.

Bien que JSON et YAML soient maintenant privilégiés pour leur simplicité, XML reste prévalent dans de nombreux systèmes d'entreprise et systèmes hérités. Dans Lua, la gestion native du XML n'est pas intégrée car Lua est conçu pour être petit et extensible à travers des modules.

Les bibliothèques XML pour Lua, comme LuaXML, xml2lua et d'autres, comblent cette lacune. LuaXML fournit un lecteur et écrivain XML léger, tandis que xml2lua utilise une approche événementielle similaire aux analyseurs SAX. Ces bibliothèques sont généralement mises en œuvre en Lua pur pour la portabilité, tandis que certaines peuvent s'appuyer sur C pour la performance.

En ce qui concerne la performance et l'utilisation de la mémoire, les bibliothèques XML de Lua peuvent ne pas être aussi rapides que celles dans les langages avec support natif. Cependant, pour la plupart des cas d'utilisation en Lua, en particulier dans le développement de jeux ou les scripts pour systèmes embarqués, ces bibliothèques font un bon travail sans surcharger le système.

## Voir aussi
- LuaXML sur GitHub : https://github.com/LuaDist/luaxml
- xml2lua sur GitHub : https://github.com/manoelcampos/xml2lua
- Liste des bibliothèques sur Lua.org : https://lua-users.org/wiki/LibrariesAndBindings
