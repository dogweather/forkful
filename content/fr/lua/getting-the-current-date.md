---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Récupérer la date actuelle est un procédé courant en programmation pour avoir le jour, le mois et l'année actuels. Les programmeurs font cela pour diverses raisons comme le suivi des événements, la tenue des journaux (logs) ou la datation des données.

## Comment faire:
En Lua, obtenir la date actuelle est simple. Utilisez le module `os` et sa fonction `date`.

```Lua
-- Récupérer la date actuelle
date_actuelle = os.date("*t")

-- Afficher la date
print(date_actuelle.year .. "/" .. date_actuelle.month .. "/" .. date_actuelle.day)
```

Exemple de sortie :
```Lua
2022/3/1
```

## Plongée profonde
Historiquement, Lua s’appuyait sur les bibliothèques C pour des opérations comme obtenir la date et l’heure. C'est pourquoi le module `os` existe. En termes d'alternatives, vous pouvez utiliser d'autres bibliothèques Lua telles que `luadate` si vous needs plus de fonctionnalités. Concernant les détails d'implémentation, `os.date("*t")` renvoie une table avec des champs pour l'année, le mois, le jour et ainsi de suite.

## Voir Aussi
Pour plus d'informations sur le module `os` et la manipulation des dates en Lua, consultez les ressources suivantes :

1. [Documentation Lua](https://www.lua.org/pil/22.1.html)
2. [Bibliothèque Luadate](https://github.com/Tieske/date) 
3. [Tutoriel sur les bibliothèques standards Lua](https://www.tutorialspoint.com/lua/lua_standard_libraries.htm)