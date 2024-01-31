---
title:                "Écriture d'un fichier texte"
date:                  2024-01-19
simple_title:         "Écriture d'un fichier texte"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
En Lua, écrire dans un fichier texte permet de sauvegarder des données persistentes. Les programmeurs utilisent cette fonction pour stocker des configurations, des logs ou partager des informations entre différents programmes.

## How to:
Pour écrire dans un fichier texte, il suffit de quelques lignes :

```Lua
-- Ouvrir un fichier en mode écriture
local fichier = io.open("exemple.txt", "w")
-- Verifie si le fichier est accessible
if not fichier then
  error("Impossible d'ouvrir le fichier.")
end

-- Écrire du texte
fichier:write("Hello, monde!\n")

-- Toujours fermer le fichier à la fin
fichier:close()
```

Après avoir exécuté ce code, vous trouverez un fichier `exemple.txt` contenant le texte "Hello, monde!".

## Deep Dive
Ecrire dans un fichier avec Lua est introduit dès les premières versions du langage. Historiquement, Lua fournissait des méthodes simples pour l'I/O (entrées/sorties) qui continue à être utilisées. Des alternatives, comme `io.output` ou les bibliothèques tiers `lfs` (Lua File System), existent. L'importance de bien gérer l'accès au fichier (comme utiliser `close`) aide à prévenir les fuites de ressources.

## See Also
- La documentation officielle pour la manipulation de fichiers en Lua : https://www.lua.org/pil/21.2.html
- Lua File System pour des fonctionnalités avancées : http://keplerproject.github.io/luafilesystem/
- Un guide sur le système d'entrées/sorties en Lua : https://www.tutorialspoint.com/lua/lua_file_io.htm
