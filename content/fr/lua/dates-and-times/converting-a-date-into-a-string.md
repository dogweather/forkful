---
aliases:
- /fr/lua/converting-a-date-into-a-string/
date: 2024-01-20 17:36:57.344496-07:00
description: "Convertir une date en cha\xEEne de caract\xE8res permet de la manipuler\
  \ comme texte, simplifiant l\u2019affichage et le stockage. Les programmeurs font\
  \ cette\u2026"
lastmod: 2024-02-18 23:09:08.982330
model: gpt-4-1106-preview
summary: "Convertir une date en cha\xEEne de caract\xE8res permet de la manipuler\
  \ comme texte, simplifiant l\u2019affichage et le stockage. Les programmeurs font\
  \ cette\u2026"
title: "Conversion d'une date en cha\xEEne de caract\xE8res"
---

{{< edit_this_page >}}

## What & Why?
Convertir une date en chaîne de caractères permet de la manipuler comme texte, simplifiant l’affichage et le stockage. Les programmeurs font cette conversion pour l'interopérabilité et la lisibilité humaine.

## How to:
Utilisons `os.date` pour formater une date :

```Lua
local date_actuelle = os.date("*t") -- Obtenir la date et l'heure courantes.
local texte_date = os.date("%A, %d %B %Y", os.time(date_actuelle))

print(texte_date) -- Affiche par exemple "mardi, 29 mars 2023"
```

Afficher seulement l'heure :

```Lua
local heure = os.date("%H:%M:%S")
print(heure) -- Affiche par exemple "14:55:07"
```

## Deep Dive
En Lua, la fonction `os.date` base sa flexibilité sur des formats inspirés de la norme ANSI C. Auparavant, convertir des dates nécessitait des bibliothèques supplémentaires ou des implémentations maison.

Alternatives :

- `os.time` peut récupérer un timestamp simple si les détails ne sont pas nécessaires.
- Des bibliothèques externes offrent plus d'options si `os.date` ne suffit pas.

Détails d'implémentation :

- Utilisez le tableau renvoyé par `os.date("*t")` pour un contrôle total sur les éléments de date.
- Prenez en compte la localisation pour la présentation des noms de jours et de mois.

## See Also
- [Lua 5.4 Reference Manual: os.date](https://www.lua.org/manual/5.4/manual.html#6.9)
- [GitHub: luadate – A more robust date library for Lua](https://github.com/Tieske/date)
