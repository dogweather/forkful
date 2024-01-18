---
title:                "Analyser le html"
html_title:           "Lua: Analyser le html"
simple_title:         "Analyser le html"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire ?

Le "parsing" HTML est le processus de conversion de code HTML en une structure de données utilisable par un programme. Les programmeurs le font souvent lorsqu'ils doivent extraire des informations spécifiques d'une page Web ou automatiser la recherche de certaines données en ligne.

## Comment faire :

Voici un exemple de code Lua qui utilise la bibliothèque "lhtml" pour parcourir une page Web et extraire tous les liens présents dans le code HTML :

```Lua
local lhtml = require "lhtml"
local document = lhtml.parse("<html><body><a href='https://example.com'>Lien 1</a><a href='https://google.com'>Lien 2</a></body></html>")

for _, link in ipairs(document:getElementsByTagName("a")) do
  print(link.attributes.href) -- affiche les liens extraits
end
```

Sortie :

```
https://example.com
https://google.com
```

## Approfondissement :

L'analyse du HTML est devenue essentielle pour les développeurs Web afin de gérer des quantités de données importantes et d'automatiser certaines tâches. Il existe également d'autres bibliothèques et outils tels que "LuaHTMLParser" et "LuaSoup" qui offrent des fonctionnalités similaires et peuvent être utilisés en fonction des préférences du programmeur.

## Voir aussi :

- La documentation de la bibliothèque "lhtml" : https://github.com/silentbicycle/lhtml
- La bibliothèque "LuaHTMLParser" : https://github.com/mpeterv/luahtmlparser
- La bibliothèque "LuaSoup" : https://github.com/mingodad/lua-soup