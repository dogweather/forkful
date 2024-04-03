---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:12.303497-07:00
description: "R\xE9cup\xE9rer la date actuelle en programmation est une t\xE2che cruciale\
  \ pour de nombreuses applications, y compris la journalisation, le marquage temporel\
  \ des\u2026"
lastmod: '2024-03-13T22:44:57.948183-06:00'
model: gpt-4-0125-preview
summary: "R\xE9cup\xE9rer la date actuelle en programmation est une t\xE2che cruciale\
  \ pour de nombreuses applications, y compris la journalisation, le marquage temporel\
  \ des \xE9v\xE9nements ou la planification des t\xE2ches."
title: Obtenir la date actuelle
weight: 29
---

## Quoi et Pourquoi ?

Récupérer la date actuelle en programmation est une tâche cruciale pour de nombreuses applications, y compris la journalisation, le marquage temporel des événements ou la planification des tâches. En Lua, cette fonctionnalité permet aux programmeurs de gérer les opérations de date et d'heure de manière transparente au sein de leurs applications, garantissant que leur logiciel peut interagir efficacement avec les données en temps réel.

## Comment faire :

Lua fournit la fonction `os.date` pour obtenir la date et l'heure actuelles. La fonction peut être utilisée sans arguments pour obtenir une chaîne formatée ou avec des spécificateurs de format pour personnaliser la sortie. Voici comment l'utiliser :

```lua
-- Obtenir la date et l'heure actuelles comme une chaîne formatée
print(os.date())  -- par exemple, Thu Mar  3 14:02:03 2022

-- Personnaliser le format de sortie
-- %Y pour l'année, %m pour le mois, %d pour le jour, %H pour l'heure, %M pour les minutes
print(os.date("%Y-%m-%d %H:%M"))  -- par exemple, 2022-03-03 14:02
```

Pour des manipulations de date et d'heure plus sophistiquées, Lua n'a pas de bibliothèques intégrées aussi riches que certains autres langages de programmation. Cependant, vous pouvez utiliser des bibliothèques tierces comme `lua-date` (https://github.com/Tieske/date). Cette bibliothèque offre des fonctionnalités plus complètes pour manipuler les dates et les heures. Voici comment vous pourriez l'utiliser :

Tout d'abord, assurez-vous d'avoir installé la bibliothèque `lua-date`. Vous pouvez généralement l'installer en utilisant LuaRocks avec la commande suivante :

```bash
luarocks install lua-date
```

Ensuite, vous pouvez l'utiliser dans votre script Lua de cette manière :

```lua
local date = require("date")

-- Créer un objet date pour la date et l'heure actuelles
local now = date()

print(now:fmt("%Y-%m-%d %H:%M:%S"))  -- par exemple, 2022-03-03 14:02:03
```

Cet exemple démontre la création d'un objet `date` représentant le moment actuel, que vous pouvez ensuite formater de manière similaire à la fonction `os.date` mais avec une flexibilité et des options supplémentaires fournies par la bibliothèque `lua-date`.
