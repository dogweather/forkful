---
title:                "Travailler avec JSON"
html_title:           "Lua: Travailler avec JSON"
simple_title:         "Travailler avec JSON"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/working-with-json.md"
---

{{< edit_this_page >}}

## Qu'est-ce que le JSON? 
Le JSON (JavaScript Object Notation) est un format de données léger et facile à lire pour échanger des données entre différents programmes. Les programmeurs utilisent le JSON pour stocker et transférer des données structurées telles que des tableaux et des objets.

## Comment faire: 
Pour utiliser le JSON en Lua, nous avons besoin d'une bibliothèque appelée "dkjson". Voici un exemple simple de code pour créer un objet JSON: 

```Lua
local json = require("dkjson")

-- Création d'un objet JSON
local obj = {name="John", age=25, hobbies={"reading", "running"}}

-- Convertir l'objet en JSON
local output = json.encode(obj)

print(output)
``` 

Output:
``` 
{"age":25,"hobbies":{"1":"reading","2":"running"},"name":"John"}
```

## Plongée en profondeur: 
Le JSON a été créé par Douglas Crockford en 2001 pour fournir une alternative légère au format XML. Il est devenu très populaire en raison de sa simplicité et de sa compatibilité avec de nombreux langages de programmation. En plus de Lua, le JSON est également largement utilisé dans d'autres langages tels que JavaScript et Python.

Il existe d'autres bibliothèques pour travailler avec le JSON en Lua, comme "cjson" et "lua-json". Chacune a ses propres avantages et inconvénients, donc il est important de choisir celle qui répond le mieux à vos besoins.

Pour utiliser le JSON en Lua, la bibliothèque "dkjson" utilise les fonctions "encode" et "decode" pour convertir les données entre les objets Lua et le format JSON. Les tableaux Lua sont convertis en tableaux JSON tandis que les tables Lua avec des clés numériques sont converties en objets JSON. 

## Voir Aussi: 
Vous pouvez en savoir plus sur la syntaxe et les règles du JSON sur le site officiel de JSON. Vous pouvez également trouver des tutoriels utiles et des exemples plus avancés pour travailler avec le JSON en Lua sur des forums et des blogs de programmation en ligne.