---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Créer un fichier temporaire correspond à la création d'un fichier à court terme, généralement utilisé pour stocker les données temporairement avant leur transfert à un emplacement plus permanent. Les programmeurs le font pour gérer l'espace disque efficacement et éviter de surcharger la mémoire.

## Comment faire :
Voici comment créer et utiliser un fichier temporaire en Lua :

```Lua
local tempfile = os.tmpname()
local file = io.open(tempfile, "w")
file:write("Ceci est un texte d'essai.")
file:close()

file = io.open(tempfile, "r")
local content = file:read("*a")
file:close()
print(content)
os.remove(tempfile)
```

Cela crée un fichier temporaire, écrit "Ceci est un texte d'essai." dedans, le lit ensuite et affiche son contenu, et enfin, le supprime.

## Plongée profonde
Historiquement, la création de fichiers temporaires tient ses racines des systèmes UNIX où `/tmp` était couramment utilisé pour de tels fichiers. En Lua, bien que `os.tmpname` soit couramment utilisé, il existe des alternatives comme le module `lfs` (LuaFileSystem) qui fournit `lfs.tempdir()`. Gardez à l'esprit que `os.tmpname` crée un nom unique pour le fichier temporaire, mais ne le crée pas réellement, c'est à vous de l'ouvrir et de le manipuler.

## Voir aussi
Pour plus d'informations sur le maniement de fichiers en Lua, consultez les liens suivants :
   
- [Programming in Lua: Files and I/O](http://www.lua.org/pil/21.html)
- [Lua-Users Wiki: File Input/Output](http://lua-users.org/wiki/FileInputOutput)