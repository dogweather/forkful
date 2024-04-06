---
date: 2024-01-20 17:42:40.188453-07:00
description: "Comment faire : La suppression de caract\xE8res selon un motif est possible\
  \ gr\xE2ce aux expressions r\xE9guli\xE8res, introduites dans les ann\xE9es 1950.\
  \ En Lua, on\u2026"
lastmod: '2024-04-05T21:53:59.392263-06:00'
model: gpt-4-1106-preview
summary: "La suppression de caract\xE8res selon un motif est possible gr\xE2ce aux\
  \ expressions r\xE9guli\xE8res, introduites dans les ann\xE9es 1950."
title: "Suppression de caract\xE8res correspondant \xE0 un motif"
weight: 5
---

## Comment faire :
```Lua
-- Exemple simple pour supprimer les chiffres d'une chaîne
local chaine = "Lua 5.4 c'est super en 2023!"
local motif = "%d+" -- motif pour identifier des chiffres
local chaine_sans_chiffres = chaine:gsub(motif, "")
print(chaine_sans_chiffres) -- Affiche : Lua c'est super en !

-- Supprimer les espaces superflus
local texte = "Trop   d'espaces   ici !"
local texte_propre = texte:gsub("%s+", " ")
print(texte_propre) -- Affiche : Trop d'espaces ici !

-- Exclure des caractères spécifiques
local adresse = "user@domain.com"
local adresse_sans_arobase = adresse:gsub("@", "")
print(adresse_sans_arobase) -- Affiche : userdomain.com
```

## Plongée Profonde :
La suppression de caractères selon un motif est possible grâce aux expressions régulières, introduites dans les années 1950. En Lua, on manipule des motifs avec une syntaxe similaire mais plus simple. Les alternatives incluent l'utilisation de fonctions de bibliothèque ou d'API, comme `stringx` pour des besoins plus complexes. La fonction `gsub` de Lua est puissante car elle permet de remplacer des occurrences de motifs par d'autres chaînes ou des fonctions.

## Voir Aussi :
- [Lua 5.4 Reference Manual: Patterns](https://www.lua.org/manual/5.4/manual.html#6.4.1)
- [Programming in Lua (4th edition)](https://www.lua.org/pil/contents.html)
