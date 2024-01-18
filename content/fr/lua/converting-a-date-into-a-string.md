---
title:                "Convertir une date en une chaîne de caractères"
html_title:           "Lua: Convertir une date en une chaîne de caractères"
simple_title:         "Convertir une date en une chaîne de caractères"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Convertis une date en une chaîne de caractères signifie simplement transformer une date dans un format lisible pour les humains, comme "26 décembre 2020", plutôt que le format numérique habituel. Les programmeurs font cela pour faciliter la lecture et la compréhension des dates dans leur code.

## Comment faire:

```Lua
local date = os.date("%d/%m/%Y")
print(date)
```

Cela utilisera la fonction os.date pour récupérer la date actuelle et la convertir en une chaîne de caractères dans le format spécifié (%d pour le jour, %m pour le mois et %Y pour l'année). Ensuite, il l'imprimera dans la console.

Output: 26/12/2020

## Plongée en profondeur:

- Contexte historique: Dans le passé, les dates étaient stockées sous forme de chiffres, ce qui rendait difficile la lecture et la compréhension des dates pour les humains. Grâce à la conversion en chaînes de caractères, il est désormais plus facile de représenter les dates dans un format compréhensible.

- Alternatives: Une alternative à la fonction os.date est d'utiliser une librairie externe telle que "date.lua". Cette librairie offre des fonctionnalités avancées pour manipuler les dates, tout en conservant une syntaxe simple à utiliser.

- Détails d'implémentation: La fonction os.date utilise la bibliothèque C "strftime" pour effectuer la conversion de la date en chaîne de caractères. Elle prend en charge une variété de formats pour personnaliser la présentation de la date.

## A voir également:

- [Documentation officielle de la fonction os.date Lua](https://www.lua.org/manual/5.4/manual.html#6.9)
- [Librairie date.lua](https://github.com/Tieske/date)
- [Documentation de la bibliothèque C "strftime"](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)