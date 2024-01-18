---
title:                "Recherche et remplacement de texte"
html_title:           "Lua: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Rechercher et remplacer du texte est une tâche courante pour les programmeurs. Cela consiste à trouver et remplacer des morceaux de texte dans un fichier ou une chaîne de caractères. Les programmeurs le font pour modifier rapidement de grandes quantités de texte, corriger des erreurs ou mettre à jour des noms de variables.

## Comment faire :

```Lua
-- Exemple de recherche et remplacement dans une chaîne de caractères
local message = "Bonjour, mon nom est Lua."
message = string.gsub(message, "Lua", "Maxime")
print(message)
```
Output: Bonjour, mon nom est Maxime.

```Lua
-- Exemple de recherche et remplacement dans un fichier
local file = io.open("monfichier.txt", "r") -- Ouvrir le fichier en mode lecture
local contenu = file:read("*all") -- Lire tout le contenu du fichier
file:close() -- Fermer le fichier
contenu = string.gsub(contenu, "Lua", "Maxime") -- Rechercher et remplacer dans le contenu
file = io.open("monfichier.txt", "w") -- Ouvrir le fichier en mode écriture
file:write(contenu) -- Écrire le contenu modifié dans le fichier
file:close() -- Fermer le fichier
```

## Plongée en profondeur :

Les recherches et remplacements de texte étaient autrefois effectués à la main, avec la fonction "Find and Replace" dans un éditeur de texte. Avec l'émergence de langages de programmation, les programmeurs ont inventé des outils pour automatiser cette tâche. Il existe également des outils de recherche et remplacement spécifiques à certains langages, tels que la commande "sed" en UNIX.

## À voir également :

Pour en savoir plus sur les fonctions de recherche et remplacement disponibles en Lua, consultez la documentation officielle : [https://www.lua.org/manual/5.3/manual.html#6.4.1](https://www.lua.org/manual/5.3/manual.html#6.4.1)

Pour découvrir d'autres manières d'optimiser votre code avec Lua, jetez un coup d'œil à ces tutoriels : [https://www.tutorialspoint.com/lua/](https://www.tutorialspoint.com/lua/)