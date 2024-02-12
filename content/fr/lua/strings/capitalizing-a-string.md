---
title:                "Mettre en majuscule une chaîne"
aliases: - /fr/lua/capitalizing-a-string.md
date:                  2024-02-03T19:05:46.817853-07:00
model:                 gpt-4-0125-preview
simple_title:         "Mettre en majuscule une chaîne"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Mettre en majuscule une chaîne de caractères consiste à modifier le premier caractère de chaque mot d'une phrase pour le mettre en majuscule, tout en s'assurant que les autres restent en minuscule. Cette technique est couramment utilisée pour formater le texte afin d'obtenir un rendu plus professionnel ou lisible, comme pour préparer des titres ou des saisies d'utilisateur pour l'affichage.

## Comment faire :
Lua n'a pas de fonction intégrée pour mettre les chaînes en majuscule, mais vous pouvez facilement accomplir cette tâche en utilisant des fonctions de manipulation de chaînes de base. Voici une fonction simple pour mettre la première lettre d'un seul mot en majuscule :

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hello"))  -- Sortie : Hello
```

Pour mettre chaque mot d'une phrase en majuscule, vous pouvez diviser la phrase en mots, mettre chacun d'eux en majuscule, puis les réunir :

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hello world from lua"))  -- Sortie : Hello World From Lua
```

Si vous travaillez sur un projet où la performance est clé et que vous avez besoin de capacités de manipulation de chaîne plus avancées, pensez à utiliser une bibliothèque tierce comme `Penlight`. Penlight améliore Lua avec des fonctions de manipulation de chaînes plus polyvalentes, parmi d'autres utilitaires :

```lua
-- En supposant que Penlight est installé :
local pl = require("pl.stringx")
local text = "hello lua users"
text = pl.capitalized(text)
print(text)  -- Sortie : Hello lua users

-- Note : La fonction capitalized de Penlight met seulement en majuscule le premier mot.
-- Pour mettre en majuscule chaque mot, vous devriez toujours implémenter une solution personnalisée ou explorer d'autres bibliothèques.
```
