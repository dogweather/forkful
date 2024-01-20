---
title:                "Convertir une chaîne en minuscules"
html_title:           "Arduino: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La conversion d'une chaîne en minuscules est une tâche répandue en programmation. C'est utile pour uniformiser les données, éviter les erreurs de casse et faciliter le tri ou la comparaison.

## Comment Faire :

En Lua, nous utilisons la méthode ``` string.lower()```. Voilà comment ça marche :

```Lua
-- Initialiser une chaîne en majuscules
local str = "Bonjour, Comment ça va?"

-- Convertir la chaîne en minuscules
local lowerStr = string.lower(str)

-- Afficher le résultat
print(lowerStr)
```

La sortie sera :

```Lua
"bonjour, comment ça va?"
```

Facile, non?

## Plus de détails

Lua a hérité de la fonction ```string.lower()``` de la bibliothèque C standard, qui a été introduite dans les premières versions de C. Bien qu'il existe des alternatives, comme la transformation de chaque caractère individuellement, utiliser la méthode intégrée est le moyen le plus efficace et le plus simple de convertir une chaîne en minuscules.

C'est important de noter que ```string.lower()``` fonctionne avec les chaînes ASCII. Si vous travaillez avec des caractères non-ASCII (comme des accents, des umlauts, etc.), vous pourriez rencontrer des problèmes.

## Voir Aussi :

- Docs Lua sur string.lower : https://www.lua.org/manual/5.4/manual.html#6.5
- Cours sur les chaînes de caractères en Lua : https://learnxinyminutes.com/docs/fr-fr/lua-fr/