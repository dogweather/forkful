---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# La recherche et le remplacement de texte en Lua
_A tout moment, la programmation peut surprendre._

## Qu'est-ce que c'est et pourquoi?
La recherche et le remplacement de texte en programmation permettent de trouver une chaîne de caractères spécifique dans un texte et de la remplacer par une autre. Les programmeurs le font pour modifier, mettre à jour ou corriger des données dans leur code.

## Comment faire:
Regardons un exemple de base d'une telle opération en Lua. Voici comment on peut changer "Bonjour le monde" en "Bonjour Univers".

```lua
texte = "Bonjour le monde"
texte = string.gsub(texte, "le monde", "l'Univers")
print(texte) -- sortie: "Bonjour l'Univers"
```

Dans cet exemple, nous utilisons la fonction `string.gsub`. Cette fonction nécessite trois paramètres: la chaîne originale, la sous-chaîne à rechercher, et la sous-chaîne par laquelle la remplacer.

## Plongée profonde
Historiquement, la recherche et le remplacement de texte est une pratique qui remonte aux origines de la programmation. C'est une nécessité constante car le code évolue et change constamment, que ce soit pour corriger des erreurs, améliorer l'optimisation ou simplement refléter des données mises à jour.

En Lua, alternativement à `string.gsub`, vous pouvez utiliser `string.match` pour seulement trouver une sous-chaîne sans la remplacer.

En termes de détails d'implémentation, notez que `string.gsub` remplace toutes les occurrences de la sous-chaîne de recherche. Si vous voulez limiter le nombre de remplacements, passez un quatrième argument à `string.gsub`. Par exemple, `string.gsub(texte, "le monde", "l'Univers", 1)`

## Voir aussi:
Pour aller plus loin dans votre apprentissage de Lua et mieux comprendre sa gestion de texte, consultez ces sources :

- [Le Manuel de Référence Lua 5.4](https://www.lua.org/manual/5.4/)
- [La documentation de la bibliothèque string de Lua](http://lua-users.org/wiki/StringLibraryTutorial)