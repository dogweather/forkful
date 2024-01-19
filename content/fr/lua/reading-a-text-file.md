---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lire un Fichier Texte avec Lua

## Quoi & Pourquoi ?
Lire un fichier texte en programmation consiste à extraire les données d'un fichier texte existant. Les programmeurs le font pour manipuler, analyser et utiliser ces données dans leur code.

## Comment faire :
Saisissons directement les points essentiels. Voici comment lire un fichier texte en Lua:

```Lua
fichier = io.open("mon_fichier.txt", "r") -- ouvre le fichier en mode lecture
if fichier then -- vérifier si le fichier a été ouvert correctement
    contenu = fichier:read("*a") -- lit tout le fichier
    fichier:close() -- ferme le fichier
end
print(contenu) -- affiche le contenu du fichier
```

Assurez-vous que "mon_fichier.txt" existe dans le même répertoire que votre script Lua. Ce code ouvrira "mon_fichier.txt", lira tout son contenu, le fermera et finalement, imprimera le contenu.

## Plongée Profonde
Historiquement, la lecture des fichiers en Lua est basée sur les principes de bas niveau du langage C, mais avec une interface simplifiée. 

Deux alternatives à la fonction `io.open` sont `io.input` (qui définit le fichier comme entrée par défaut) et `io.lines` (qui vous permet de lire le fichier ligne par ligne). 

Dans `fichier:read("*a")`, le "*a" est un modificateur qui indique de lire tout le fichier. D'autres modificateurs sont disponibles, comme "*l" pour lire une ligne ou "*n" pour lire un nombre.

## Voir Aussi
* Documentation officielle de Lua sur les E/S fichiers: http://www.lua.org/manual/5.3/manual.html#6.9
* Tutoriel interactif sur les fichiers Lua: https://www.tutorialspoint.com/lua/lua_file_io.htm