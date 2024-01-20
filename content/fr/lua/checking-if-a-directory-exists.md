---
title:                "Vérifier si un répertoire existe"
html_title:           "Lua: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Vérifier si un répertoire existe est une tâche courante pour les développeurs, où ils vérifient l'existence d'un chemin de répertoire spécifique. Ils le font pour éviter les erreurs à l'exécution lors de la lecture, de l'écriture ou de la manipulation des données stockées dans ce répertoire.

## Comment faire :
Le code suivant montre comment vérifier si un répertoire existe en Lua.

``` Lua
local lfs = require('lfs')

local function directory_exists(directory)
    -- Check if the directory exists in Lua
    if lfs.attributes(directory, "mode") == "directory" then
        -- Directory exists
        return true
    else
        -- Directory does not exist
        return false
    end
end

-- Usage
if directory_exists('myDirectory') then
    print('Directory exists.')
else
    print('Directory does not exist.')
end
```

Si le répertoire existe, la sortie sera :
``` Lua
Directory exists.
```

Si le répertoire n'existe pas, la sortie sera :
``` Lua 
Directory does not exist.
```

## Plongée en profondeur
Historiquement, il n'y avait pas de moyen intégré de vérifier l'existence d'un répertoire en Lua, il fallait donc recourir à des méthodes indirectes. Plus tard, la bibliothèque LuaFileSystem a été introduite pour résoudre ce problème.

Il existe également une alternative en utilisant les commandes du système d'exploitation (os library) en Lua, bien que cette méthode ne soit pas recommandée car elle n'est pas portable.

Le détail de l'implémentation "dir_exists" utilise la fonction "lfs.attributes" qui renvoie un tableau contenant les détails du dossier si le dossier existe, ou renvoit nil si le dossier n'existe pas.

## Voir aussi 
Pour plus d'informations, visitez les liens ci-dessous :

2. [Lua OS Library](https://www.lua.org/pil/23.1.html)
3. [Lua tutorials on File I/O](https://www.tutorialspoint.com/lua/lua_file_io.htm)