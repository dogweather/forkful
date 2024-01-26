---
title:                "Lecture d'un fichier texte"
date:                  2024-01-20T17:54:48.580955-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture d'un fichier texte"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Lire un fichier texte, c'est extraire son contenu pour que votre programme Lua puisse l'utiliser. On fait ça pour des tas de raisons – charger des configurations, analyser des données, ou juste afficher du texte à l'utilisateur.

## Comment faire :

```Lua
-- Ouvrir un fichier en mode lecture
local fichier = io.open("monfichier.txt", "r") 

-- Vérifier que le fichier existe
if fichier then
    -- Lire tout le contenu du fichier
    local contenu = fichier:read("*a") 
    print(contenu)

    -- Toujours fermer le fichier à la fin
    fichier:close()
else
    print("Erreur lors de l'ouverture du fichier.")
end
```

Résultat :
```
Bonjour, voici le texte de votre fichier !
```

## Exploration plus profonde

Historiquement, lire des fichiers en Lua s'est toujours appuyé sur la bibliothèque `io`. Elle est simple mais puissante. Bien sûr, il y a d'autres moyens, comme la bibliothèque `lfs` (LuaFileSystem) pour des fonctionnalités plus avancées, comme parcourir des dossiers. En termes de détails d'implémentation, `io.open` renvoie un 'file handle', un objet qu'on utilise pour lire et écrire dans le fichier. `read("*a")` lit tout le fichier (`*a` signifie "all"). Il y a d'autres options : `*l` pour une ligne, `*n` pour un nombre, et ainsi de suite.

## Voir également

- La documentation de Lua sur les E/S de fichiers : http://www.lua.org/manual/5.4/manual.html#6.8
- Tutoriel Lua sur le File I/O: https://www.tutorialspoint.com/lua/lua_file_io.htm
- LuaFileSystem pour plus de contrôle sur les systèmes de fichiers : https://keplerproject.github.io/luafilesystem/
