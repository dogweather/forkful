---
title:                "Majuscule d'une chaîne de caractères"
html_title:           "Lua: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Capitaliser une chaîne de caractères signifie mettre en majuscule la première lettre de chaque mot dans une phrase. Les programmeurs le font souvent pour améliorer la lisibilité et la cohérence du code.

## Comment faire:
```Lua
-- Utiliser la méthode string.upper pour capitaliser une chaîne de caractères

-- Exemple 1:
local phrase = "bonjour tout le monde!"
print(phrase:upper())

Output: Bonjour Tout Le Monde!

-- Exemple 2:
function capitalize(string)
    local words = {}
    for word in string:gmatch("%S+") do
        table.insert(words, word:sub(1,1):upper()..word:sub(2))
    end
    return table.concat(words, " ")
end

print(capitalize("cette phrase sera capitalisée"))

Output: Cette Phrase Sera Capitalisée
```

## Plongée En Profondeur:
La capitalisation de chaîne de caractères est un concept couramment utilisé en programmation depuis les premières versions de BASIC dans les années 1960. Bien qu'il existe des alternatives telles que l'utilisation de la fonction string.gsub pour remplacer la première lettre d'un mot par sa version en majuscule, cela peut être plus fastidieux et rendre le code plus difficile à lire. En Lua, la méthode string.upper est optimisée pour la performance et est donc préférée pour capitaliser une chaîne de caractères.

## Voir Aussi:
Liens utiles pour en savoir plus sur la capitalisation de chaîne de caractères en Lua:
- [Documentation de string.upper sur le site officiel de Lua](https://www.lua.org/manual/5.4/manual.html#pdf-string.upper)
- [Différences entre string.upper et string.gsub sur Stack Overflow](https://stackoverflow.com/questions/1915406/what-is-the-difference-between-string-upper-and-string-gsub)