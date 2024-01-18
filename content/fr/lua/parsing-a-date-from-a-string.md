---
title:                "Analyser une date à partir d'une chaîne de caractères."
html_title:           "Lua: Analyser une date à partir d'une chaîne de caractères."
simple_title:         "Analyser une date à partir d'une chaîne de caractères."
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Parsing une date à partir d'une chaîne de caractères est le processus de convertir une date écrite sous forme de texte en une structure de données pouvant être traitée par un programme informatique. Les programmeurs le font souvent pour extraire des informations spécifiques d'une date, comme le jour, le mois ou l'année.

## Comment faire:

```
-- Exemple 1: Parse une date à partir d'une chaîne de caractères avec un format spécifique
local date = "12/07/2020" -- Chaîne de caractères à parser
local format = "%d/%m/%Y" -- Format de la date (jour/mois/année)
local parsedDate = os.date(format, date) -- Fonction os.date() utilisée pour effectuer le parsing
print("Jour: " .. parsedDate.day) -- Imprime "Jour: 12"
print("Mois: " .. parsedDate.month) -- Imprime "Mois: 07"
print("Année: " .. parsedDate.year) -- Imprime "Année: 2020"

-- Exemple 2: Parser la date actuelle à partir d'une chaîne de caractères sans format spécifique
local date = "Sun Jul 12 2020" -- Chaîne de caractères à parser
local parsedDate = os.date("*t", date) -- Fonction os.date() utilisée avec le format "*t" pour récupérer les informations de date et heure actuelles
print("Jour de la semaine: " .. parsedDate.wday) -- Imprime "Jour de la semaine: 1" (dimanche)
print("Mois: " .. parsedDate.month) -- Imprime "Mois: 7"
print("Année: " .. parsedDate.year) -- Imprime "Année: 2020"
```

## Plongée en profondeur:

Parser des dates à partir de chaînes de caractères est une pratique courante en programmation, surtout en ce qui concerne le traitement de données en provenance de différentes sources. Les développeurs peuvent également utiliser des bibliothèques tierces pour faciliter le parsing de dates, telles que la bibliothèque "date" pour Lua. Il existe également des méthodes alternatives pour stocker des dates en tant que nombres ou utiliser des propriétés de table pour chaque composante de la date.

## Voir aussi:

- Documentation officielle de l'utilisation de la fonction os.date en Lua: https://www.lua.org/manual/5.3/manual.html#pdf-os.date
- Bibliothèque "date" pour Lua: https://keplerproject.github.io/luadate/
- Tutoriel pour le parsing de dates en Lua: https://www.techotopia.com/index.php/Working_with_Parsed_Date_String_Variables_in_Lua