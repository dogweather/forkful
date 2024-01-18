---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Lua: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qu'est-ce et pourquoi?
Vérifier si un répertoire existe est une tâche courante pour les programmeurs. Cela leur permet de s'assurer qu'un dossier nécessaire pour leur programme est présent avant de poursuivre l'exécution. Cela évite également les erreurs potentielles liées à l'absence d'un répertoire spécifique.

## Comment faire:
Pour vérifier si un répertoire existe en utilisant Lua, vous pouvez utiliser la fonction "lfs.attributes". Cette fonction prend un chemin en paramètre et renvoie une table avec des informations sur le chemin spécifié. En utilisant la clé "mode" de cette table, vous pouvez déterminer si le chemin existe ou non. Voici un exemple de code:

```
local lfs = require("lfs")
local path = "/chemin/vers/mon/répertoire"

local valid = lfs.attributes(path, "mode") == "directory"

if valid then
    print("Le répertoire existe.")
else
    print("Le répertoire n'existe pas.")
end
```

## Plongée en profondeur:
Dans les versions précédentes de Lua, il n'y avait pas de fonction intégrée pour vérifier si un répertoire existe. Les programmeurs devaient utiliser des astuces et des fonctions système pour réaliser cette tâche. La fonction "lfs.attributes" est maintenant disponible grâce au module "lfs" de LuaFileSystem. Il existe également d'autres méthodes pour vérifier si un répertoire existe, comme l'utilisation de la bibliothèque LuaJIT "ffi" pour appeler la fonction C "stat" et obtenir des informations sur le chemin spécifié.

## Voir aussi:
- Documentation officielle de LuaFileSystem: https://keplerproject.github.io/luafilesystem/manual.html
- Utilisation du module LFS pour vérifier si un répertoire existe en Lua: https://stackoverflow.com/questions/1614374/how-do-i-check-if-a-directory-exists-in-lua