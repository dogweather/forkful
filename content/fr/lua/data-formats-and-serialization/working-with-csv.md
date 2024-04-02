---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:42.671643-07:00
description: "Travailler avec des fichiers CSV (Comma-Separated Values ou Valeurs\
  \ S\xE9par\xE9es par des Virgules) implique de parser et de g\xE9n\xE9rer des donn\xE9\
  es textuelles\u2026"
lastmod: '2024-03-13T22:44:57.963029-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec des fichiers CSV (Comma-Separated Values ou Valeurs S\xE9\
  par\xE9es par des Virgules) implique de parser et de g\xE9n\xE9rer des donn\xE9\
  es textuelles\u2026"
title: Travailler avec CSV
weight: 37
---

## Quoi & Pourquoi ?

Travailler avec des fichiers CSV (Comma-Separated Values ou Valeurs Séparées par des Virgules) implique de parser et de générer des données textuelles organisées en lignes et colonnes, en utilisant des virgules pour séparer les valeurs individuelles. Les programmeurs s'engagent souvent dans ce processus pour faciliter l'échange de données entre différentes applications, bases de données, ou pour des tâches de traitement et d'analyse de données, en raison du support généralisé et de la simplicité du CSV.

## Comment faire :

En Lua, travailler avec des fichiers CSV peut être abordé en utilisant les opérations de base d'entrée/sortie de fichiers fournies par le langage, sans nécessiter de bibliothèques externes pour des tâches simples. Pour des opérations plus complexes, comme la gestion de cas spéciaux (par exemple, des virgules dans les valeurs), il pourrait être bénéfique d'utiliser des bibliothèques tierces comme `lua-csv`.

### Lire un fichier CSV
Voici un exemple simple pour lire un fichier CSV ligne par ligne, en divisant chaque ligne en valeurs basées sur le séparateur de virgules.

```lua
function parseCSVLine(ligne)
    local resultat = {}
    local de = 1
    local sep = ","
    local champ
    while true do
        local debut, fin = string.find(ligne, sep, de)
        if not debut then
            table.insert(resultat, string.sub(ligne, de))
            break
        end
        champ = string.sub(ligne, de, debut - 1)
        table.insert(resultat, champ)
        de = fin + 1
    end
    return resultat
end

local fichier = io.open("exemple.csv", "r")
for ligne in fichier:lines() do
    local valeurs = parseCSVLine(ligne)
    for i, v in ipairs(valeurs) do
        print(i, v)
    end
end
fichier:close()
```

**Exemple de sortie** (pour un `exemple.csv` avec contenu "nom,âge\newlineJohn Doe,30\newlineJane Doe,32"):
```
1	nom
2	âge
1	John Doe
2	30
1	Jane Doe
2	32
```

### Écrire un fichier CSV
Pour générer un fichier CSV, il suffit de construire des chaînes avec des valeurs séparées par des virgules et de les écrire dans un fichier ligne par ligne.

```lua
local donnees = {
    {"nom", "âge"},
    {"John Doe", "30"},
    {"Jane Doe", "32"}
}

local fichier = io.open("sortie.csv", "w")
for _, v in ipairs(donnees) do
    fichier:write(table.concat(v, ","), "\n")
end
fichier:close()
```

Cela créerait (ou écraserait) un fichier `sortie.csv` avec les données spécifiées.

### Utiliser lua-csv
Pour une gestion plus avancée des CSV, incluant le support des guillemets et des caractères d'échappement, la bibliothèque `lua-csv` est un choix robuste.

Premièrement, installez-la en utilisant LuaRocks :
```shell
luarocks install lua-csv
```

Ensuite, lire un fichier CSV devient aussi simple que :

```lua
local csv = require("csv")

-- Lire depuis un fichier
for champs in csv.open("exemple.csv") do
    for i, v in ipairs(champs) do
        print(i, v)
    end
end
```

Et écrire dans un CSV avec des guillemets et échappements appropriés :

```lua
local fichier = csv.open("sortie.csv", {write=true})

local donnees = {
    {"nom", "profession", "lieu"},
    {"John Doe", "Ingénieur Logiciel", "New York, NY"},
    {"Jane Doe", "Scientifique de Données", "\"San Francisco, CA\""}
}

for _, v in ipairs(donnees) do
    fichier:write(v)
end
```

Cette approche gère automatiquement les complexités telles que les virgules et guillemets à l'intérieur des valeurs.
