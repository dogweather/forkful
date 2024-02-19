---
aliases:
- /fr/lua/working-with-complex-numbers/
date: 2024-01-26 04:43:19.254417-07:00
description: "Les nombres complexes \xE9tendent l'id\xE9e de la ligne num\xE9rique\
  \ unidimensionnelle dans le plan bidimensionnel en incluant un axe imaginaire perpendiculaire.\u2026"
lastmod: 2024-02-18 23:09:08.962952
model: gpt-4-0125-preview
summary: "Les nombres complexes \xE9tendent l'id\xE9e de la ligne num\xE9rique unidimensionnelle\
  \ dans le plan bidimensionnel en incluant un axe imaginaire perpendiculaire.\u2026"
title: Manipulation des nombres complexes
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les nombres complexes étendent l'idée de la ligne numérique unidimensionnelle dans le plan bidimensionnel en incluant un axe imaginaire perpendiculaire. Les programmeurs travaillent avec eux dans des domaines tels que le traitement des signaux, la dynamique des fluides et l'ingénierie électrique, où ils sont essentiels pour représenter les oscillations et autres phénomènes.

## Comment faire :
En Lua, vous pouvez représenter les nombres complexes avec des tables. Les opérations de base impliquent d'ajouter, de soustraire, de multiplier et de diviser ces tables. Voici comment :

```lua
-- Définir deux nombres complexes en tant que tables
local complex_a = { real = 3, imag = 5 }
local complex_b = { real = 2, imag = -4 }

-- Fonction pour ajouter deux nombres complexes
local function add_complex(a, b)
  return { real = a.real + b.real, imag = a.imag + b.imag }
end

-- Exemple de sortie
print(add_complex(complex_a, complex_b))  -- { real = 5, imag = 1 }
```

## Approfondissement
Les nombres complexes existent depuis le XVIe siècle, aidant à résoudre les équations qui ne pouvaient pas être résolues avec seulement des nombres réels. Lua lui-même n'a pas de type de nombre complexe intégré. Cependant, ce n'est pas grave — vous pouvez créer vos propres manipulations de nombres complexes en utilisant des tables et des fonctions, comme montré ci-dessus. Ou, si vos besoins sont plus profonds, mettez la main sur une bibliothèque comme LuaComplex. C'est un bon choix car elle est spécifiquement conçue pour Lua et enlève le travail manuel de vos épaules. Les bibliothèques comme celle-ci optimisent souvent également les opérations sous le capot, donc elles sont plus rapides que de faire le vôtre.

## Voir Aussi
Pour des exemples plus détaillés et des opérations avancées, consultez ces liens :

- Bibliothèque LuaComplex : https://github.com/davidm/lua-complex
- Livre "Programming in Lua", pour la création de types de données personnalisés : https://www.lua.org/pil/11.1.html
- Wikipédia sur les utilisations des nombres complexes dans différents domaines : https://en.wikipedia.org/wiki/Complex_number#Applications
