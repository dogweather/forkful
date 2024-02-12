---
title:                "Extraction de sous-chaînes"
aliases:
- /fr/lua/extracting-substrings/
date:                  2024-01-20T17:46:16.710501-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraction de sous-chaînes"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extraire des sous-chaînes, c'est prendre des petits bouts de texte d'une chaîne plus grande. On fait ça pour manipuler, chercher ou analyser uniquement les parties qui nous intéressent.

## How to:
En Lua, pour extraire des sous-chaînes, on utilise `string.sub`. Voici comment ça marche :

```Lua
local phrase = "Bonjour tout le monde!"
local salutation = string.sub(phrase, 1, 7) -- Extrait 'Bonjour'
local cible = string.sub(phrase, 9, 12) -- Extrait 'tout'

print(salutation) -- Affiche 'Bonjour'
print(cible) -- Affiche 'tout'
```

On précise la position de départ et de fin pour délimiter la sous-chaîne. Si on n'indique pas la fin, ça va jusqu'au bout :

```Lua
local phrase = "Un autre exemple"
local extrait = string.sub(phrase, 4) -- Extrait 'autre exemple'

print(extrait) -- Affiche 'autre exemple'
```

Facile, non?

## Deep Dive
`string.sub` n'a pas toujours existé. Dans les premières versions de Lua, extraire des sous-chaînes était plus rudimentaire. Maintenant, `string.sub` fait partie des outils de base de Lua, et il est optimisé pour être rapide et efficace.

Il n'y a pas que `string.sub` pour jouer avec les chaînes. On a aussi les expressions régulières via la bibliothèque `string.gmatch`, ou les fonctions de recherche et remplacement comme `string.find` et `string.gsub`. Mais pour les besoins simples, `string.sub` suffit et est plus rapide.

La fonction `string.sub(s, i, j)` fonctionne en utilisant des indices en base un (le premier caractère est à l'indice 1). Si on met un indice négatif, ça compte à partir de la fin: `-1` c'est le dernier caractère, `-2` l'avant-dernier, et ainsi de suite.

## See Also
- Documentation officielle de Lua sur les chaînes de caractères : https://www.lua.org/manual/5.4/manual.html#6.4
- Tutoriel Lua sur les strings (chaînes de caractères) : https://www.tutorialspoint.com/lua/lua_strings.htm
- D'autres fonctions utiles de manipulation de chaînes : https://www.lua.org/pil/20.html

Ces liens renvoient vers des ressources en anglais. Pour l'instant, il y a moins de documentation en français sur Lua, mais ces sources sont claires et complètes.
