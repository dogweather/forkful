---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?
La sous-chaîne est une portion de chaîne que nous récupérons d'une chaîne plus grande. Les programmeurs font cela pour analyser le texte, manipuler des données et effectuer des opérations de recherche.

## Comment faire :
Voyons comment extraire une sous-chaîne en utilisant Lua. Utilisez la méthode `string.sub` :
```lua
chaine= "Programmer en Lua est amusant"
sous_chaine = string.sub(chaine, 1, 10)
print(sous_chaine)
```
Cela donnera : 
```
Programmer
```
Vous pouvez aussi utiliser les indices négatifs :
```lua
chaine= "Programmer en Lua est amusant"
sous_chaine = string.sub(chaine, -7, -1)
print(sous_chaine)
```
Cela donnera :
```
amusant
```
## Plongée en profondeur :
La fonction `string.sub` existe depuis les premières versions de Lua. Elle est implémentée de manière très efficace, mais elle n'est pas la seule façon d'extraire des sous-chaînes. Par exemple, vous pouvez également utiliser des expressions régulières avec la fonction `string.match`.

Il est important de noter que les indices de chaîne Lua commencent à 1, et pas à 0 comme dans de nombreux autres langages de programmation. De plus, Lua supporte également l'indexation négative pour compter depuis la fin d'une chaîne.

## Voir aussi :
Pour plus d'information sur la manipulation de chaînes avec Lua, consultez :
- La documentation officielle de Lua : [Strings](https://www.lua.org/manual/5.3/manual.html#6.4)
- Le tutoriel de Lua-users sur les chaînes : [String Tutorial](http://lua-users.org/wiki/StringLibraryTutorial)