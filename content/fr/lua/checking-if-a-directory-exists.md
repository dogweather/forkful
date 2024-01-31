---
title:                "Vérifier si un répertoire existe"
date:                  2024-01-20T14:57:17.645319-07:00
html_title:           "Go: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Vérifier l'existence d'un dossier, c'est comme frapper à une porte pour savoir si quelqu'un est là. Les programmeurs le font pour éviter les erreurs avant de lire ou d'écrire des fichiers, ou pour décider de créer un dossier s'il n'existe pas.

## Comment faire :
```Lua
local lfs = require("lfs")  -- On charge le module LuaFileSystem

-- Fonction pour vérifier si un dossier existe
function doesDirectoryExist(path)
    local attributes = lfs.attributes(path)
    return attributes and attributes.mode == "directory"
end

-- Exemple d'utilisation
local path = "/chemin/vers/le/dossier"
if doesDirectoryExist(path) then
    print("Le dossier existe!")
else
    print("Le dossier n'existe pas.")
end
```
L'output sera "Le dossier existe!" ou "Le dossier n'existe pas." selon le cas.

## Exploration Approfondie :
Historiquement, Lua ne concernait pas directement la gestion des fichiers. Pour cela, on utilise LuaFileSystem (lfs), une extension qui fournit ces fonctionnalités. Des alternatives incluent l'usage de commandes système via `os.execute` ou `io.popen`, mais cela peut être moins portable et sécurisé. L'approche avec `lfs.attributes` est propre parce qu'elle interagit avec le système de fichiers sans créer de dépendance avec le système d'exploitation.

## Voir Aussi :
- Documentation LuaFileSystem : http://keplerproject.github.io/luafilesystem/
- Référence Lua 5.4 (dernière version) : https://www.lua.org/manual/5.4/
- Discussions sur Stack Overflow pour des problèmes spécifiques liés aux systèmes de fichiers en Lua.
