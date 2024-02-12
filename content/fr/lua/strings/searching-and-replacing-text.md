---
title:                "Recherche et remplacement de texte"
aliases: - /fr/lua/searching-and-replacing-text.md
date:                  2024-01-20T17:58:22.057587-07:00
model:                 gpt-4-1106-preview
simple_title:         "Recherche et remplacement de texte"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Chercher et remplacer du texte, c'est un peu comme jouer à cache-cache avec des mots pour les changer en un clin d'œil. Les programmeurs s'y collent pour corriger des erreurs, mettre à jour du code ou manier des données sans se prendre la tête.

## How to:
```Lua
local originalText = "Bonjour, le monde. Ceci est une chaîne de test. Bonjour, le monde."
local searchText = "Bonjour, le monde."
local replaceText = "Salut tout le monde!"
local newText = originalText:gsub(searchText, replaceText)
print(newText)
```

```Lua
-- Sortie: Salut tout le monde! Ceci est une chaîne de test. Salut tout le monde!
```

```Lua
-- Chercher avec des motifs
local trickyText = "Les prix sont de 15€, 42€, et 33€."
local pattern = "%d+€"
local count = select(2, trickyText:gsub(pattern, "X€"))
print(trickyText:gsub(pattern, "X€"))
print("Nombre de remplacements: " .. count)
```

```Lua
-- Sortie: Les prix sont de X€, X€, et X€.
-- Nombre de remplacements: 3
```

## Deep Dive
La quête de chercher et remplacer existe depuis que les gens ont commencé à bidouiller avec du texte sur des ordis. Lua l'a rendu sympa grâce à des fonctions comme `string.gsub`. Historiquement, les sed et awk en ligne de commande faisaient le job pour les fichiers texte. En Lua, tout est une question de motifs. Ces motifs peuvent être simples ou des expressions régulières. Faut être malin pour éviter les pièges – comme "l'interprétation spéciale" des caractères quand tu bosses avec des expressions régulières.

## See Also
Pour devenir un pro, regarde par ici :
- [Lua 5.4 Reference Manual - string.gsub](http://www.lua.org/manual/5.4/manual.html#pdf-string.gsub)
- [Programming in Lua (4th edition) - Strings](https://www.lua.org/pil/21.1.html)
- Pour des cas généraux en programmation : [Regular-Expressions.info](https://www.regular-expressions.info/)
