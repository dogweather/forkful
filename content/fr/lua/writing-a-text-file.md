---
title:                "Écrire un fichier texte"
html_title:           "Lua: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que & Pourquoi?

Écrire un fichier texte en programmation consiste à créer un fichier contenant du texte qui peut être lu par un ordinateur. Les programmeurs le font pour stocker des données ou des instructions qui seront utilisées par leur programme.

## Comment faire:

```Lua
-- Ouvrir un fichier en mode écriture
local fichier = io.open("mon_fichier.txt", "w")

-- Écrire du texte dans le fichier
fichier:write("Bonjour, le monde!")

-- Fermer le fichier
fichier:close()
```

## Plongée en profondeur:

Écrire des fichiers texte a été une partie essentielle de la programmation depuis les premiers jours de l'informatique. Bien qu'il existe aujourd'hui différentes façons de stocker des données, comme les bases de données, les fichiers texte restent une méthode simple et efficace pour les programmeurs. En Lua, la fonction ```io.open()``` est utilisée pour ouvrir un fichier et la méthode ```write()``` est utilisée pour écrire du texte dans le fichier. Il est également important de fermer le fichier avec la méthode ```close()``` pour éviter les problèmes de mémoire.

## Voir aussi:

- [Documentation officielle de Lua](https://www.lua.org/manual/5.3/manual.html#pdf-io.open)
- [Tutoriel sur les fichiers en Lua](https://www.tutorialspoint.com/lua/lua_input_output.htm)
- [Vidéo explicative sur l'utilisation des fichiers en Lua](https://www.youtube.com/watch?v=ds4DI865QjI)