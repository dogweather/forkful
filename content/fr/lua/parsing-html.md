---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/parsing-html.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Analyser HTML, c'est décomposer un document HTML en ses composants pour une utilisation programmable. Les programmeurs le font pour interagir avec les éléments de la page web et manipuler les données à leur guise.

## Comment faire:

Voici un exemple basique sur comment on peut analyser du HTML en Lua en utilisant `lua-htmlparser`. Installer d'abord le package `lua-htmlparser` avec luarocks:

```Lua
luarocks install htmlparser
```

Maintenant, voici comment vous pouvez analyser un simple document HTML:

```Lua
local htmlparser = require "htmlparser"

local html = "<html><body>Hello, World!</body></html>"
local root = htmlparser.parse(html)

root("body"):each(function(i, el)
  print(el:getcontent())
end)
```

Cela imprimera:

```Lua
Hello, World!
```

## Plongée Profonde

1. En ce qui concerne le contexte historique, bien que le HTML soit largement utilisé aujourd'hui, les bibliothèques pour l'analyser en Lua n'ont été développées que récemment.
2. Pour les alternatives, souvenez-vous que vous pouvez analyser le HTML en utilisant plusieurs bibliothèques en Lua, comme `htmlparser` et `Gumbo`. Comparez et choisissez en fonction de vos nécessités.
3. Sur l'implémentation, `htmlparser` utilise une analyse basée sur des expressions régulières pour la rapidité, mais cela peut conduire à des erreurs d'analyse sur HTML mal formé. Cependant, si le HTML que vous analysez est toujours bien formé, cela ne devrait pas poser de problème.

## Voir Aussi

1. [Lua HTML Parser sur GitHub](https://github.com/msva/lua-htmlparser)
2. [Bibliothèque Gumbo en Lua](https://github.com/craigbarnes/lua-gumbo)