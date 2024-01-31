---
title:                "Manipulation des fichiers CSV"
date:                  2024-01-19
simple_title:         "Manipulation des fichiers CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Travailler avec CSV, c'est manipuler des données textuelles structurées comme une feuille de calcul, où chaque ligne est un enregistrement et chaque enregistrement est divisé par des virgules. Les programmeurs le font car c'est un format léger et universel pour l'échange de données.

## How to:
```Lua
-- Lire un fichier CSV
local function lire_csv(fichier)
    local resultat = {}
    local file = io.open(fichier, "r")

    for ligne in file:lines() do
        table.insert(resultat, ligne:split(','))
    end

    file:close()
    return resultat
end

-- Écrire dans un fichier CSV
local function ecrire_csv(donnees, fichier)
    local file = io.open(fichier, "w+")

    for _, enreg in pairs(donnees) do
        file:write(table.concat(enreg, ',') .. '\n')
    end

    file:close()
end

-- Utilisation
local donnees = lire_csv("donnees.csv")
ecrire_csv(donnees, "nouveau.csv")

-- Affichage des données
for _, enreg in ipairs(donnees) do
    print(table.concat(enreg, ', '))
end
```

## Deep Dive

Le format CSV existe depuis les premiers jours de l'informatique personnelle, servant de pont entre les tableurs et les bases de données. Des alternatives comme JSON et XML proposent plus d'expressivité, mais avec plus de lourdeur. En Lua, travailler avec CSV est direct grâce a `io` pour la lecture et écriture, et la manipulation de tables pour stocker des données.

## See Also
- Documentation Lua `io`: https://www.lua.org/manual/5.4/manual.html#6.8
- RFC 4180, la norme CSV: https://tools.ietf.org/html/rfc4180
- Site Lua-users CSV pour des bibliothèques plus avancées : http://lua-users.org/wiki/CsvUtils
