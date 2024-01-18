---
title:                "Obtenir la date actuelle"
html_title:           "Lua: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire ?

Obtenir la date actuelle est une fonctionnalité courante en programmation qui permet de récupérer la date et l'heure actuelles du système sur lequel s'exécute le code. Les programmeurs le font principalement pour enregistrer ou afficher des horodatages dans leurs applications, ainsi que pour effectuer des opérations de traitement du temps.

# Comment faire :

```
-- Exemple 1 : Utilisation de la fonction os.date()
print(os.date("%d/%m/%Y"))
-- Sortie : 25/03/2021

-- Exemple 2 : Utilisation de la fonction os.time()
local current_time = os.time()
print(current_time)
-- Sortie : 1616679650 (un nombre représentant le temps écoulé en secondes depuis le 1er janvier 1970)
```

# Plongée en profondeur :

L'obtention de la date actuelle est un mécanisme courant dans les langages de programmation modernes. Son utilité peut être retracée à l'ère des systèmes informatiques mainframe, où les horodatages étaient utilisés pour enregistrer des données dans les bases de données. En plus de la méthode présentée ci-dessus, les programmeurs peuvent également utiliser la bibliothèque *os.date* de Lua pour personnaliser le format d'affichage de la date et de l'heure. Il existe également des alternatives telles que les modules de gestion du temps spécifiques à une application.

# Voir aussi :

- [Documentation officielle du module os.date de Lua](https://www.lua.org/manual/5.4/manual.html#6.9)
- [D'autres façons d'obtenir la date actuelle en Lua](https://stackoverflow.com/questions/1889666/how-to-get-current-date-and-time-in-lua)