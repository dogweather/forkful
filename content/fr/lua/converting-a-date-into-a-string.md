---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

**## Quoi & Pourquoi?**

Changer une date en chaîne de caractères (string en anglais), c'est transformer une structure de données de date en une série de caractères. Les programmeurs le font pour faciliter l'affichage et le stockage des informations de date.

**## Comment faire:**

Voici un exemple de comment vous pouvez le faire en Lua :

```Lua
os.setlocale('fr_FR') -- configuration de la locale en français
date = os.date("*t") -- obtenir la date actuelle
date_str = os.date("%A %d %B %Y", os.time(date)) -- conversion de la date en chaîne
print(date_str) -- affichage de la chaîne
```

Dans cet exemple, la sortie pourrait être :

```
Jeudi 02 Décembre 2021
```
Elle affiche le jour de la semaine, le jour, le mois et l'année en Français.

**## Plongée profonde :**

Historiquement, la conversion date en chaîne a toujours été effectuée pour faciliter le traitement des dates. En Lua, on utilise la bibliothèque `os` qui donne un accès simple et pratique aux fonctionnalités du système d'exploitation.

Il y a d'autres façons dont vous pourriez gérer cette tâche en Lua. Par exemple, vous pouvez utiliser la fonction `string.format()` pour formater une date en fonction de vos propres besoins spécifiques.

Quant aux détails d'implémentation, la fonction `os.date()` prend deux paramètres : un format de chaîne et une heure. L'heure est généralement obtenue à partir de `os.time()`. Le format de chaîne contrôle comment l'heure est formatée.

**## Voir aussi :**

Pour plus d'informations sur les fonctions `os.date()`, `os.time()`, et `os.setlocale()`, consultez la documentation officielle de Lua : https://www.lua.org/manual/5.4/fr/

Pour des informations plus approfondies sur le formatage des dates, jetez un œil sur ces ressources :

- https://www.lua.org/pil/22.1.html
- https://en.lua-users.org/wiki/OsLibraryTutorial