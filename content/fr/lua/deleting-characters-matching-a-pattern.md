---
title:                "Supprimer les caractères correspondant à un motif"
html_title:           "Lua: Supprimer les caractères correspondant à un motif"
simple_title:         "Supprimer les caractères correspondant à un motif"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi?
L'effacement de caractères correspondant à un modèle est simplement le fait de supprimer des caractères spécifiques dans une chaîne de caractères en fonction d'un motif prédéfini. Les programmeurs le font souvent pour nettoyer ou modifier des données en vrac de manière plus efficace.

## Comment faire:
Voici un exemple de code en Lua pour montrer comment effacer des caractères correspondant à un motif donné:

```Lua 
-- Définir une chaîne de caractères
local str = "8hLi34Az%&"

-- Supprimer tous les caractères non numériques de la chaîne
str = string.gsub(str, "%D", "")

-- Afficher la chaîne après l'effacement
print(str)

-- Sortie: 834
```

## Plongée en profondeur:
Ce concept n'est pas nouveau et provient en grande partie des expressions régulières dans les langages de programmation. Cependant, Lua offre une syntaxe plus simple et plus lisible pour effectuer cette opération. Il existe également des alternatives telles que l'utilisation de boucles pour parcourir la chaîne de caractères et la supprimer caractère par caractère, mais cela peut être fastidieux et moins efficace. En termes d'implémentation, Lua utilise l'algorithme de recherche de Knuth-Morris-Pratt pour trouver le motif donné dans la chaîne.

## Voir aussi:
- [Documentation officielle de Lua](https://www.lua.org/manual/5.4/manual.html#6.4)
- [Expressions régulières en Lua](https://www.techotopia.com/index.php/Regular_Expressions_in_Lua)
- [Algorithme de recherche de Knuth-Morris-Pratt](https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm)