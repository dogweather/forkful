---
title:                "Lecture d'un fichier texte"
html_title:           "Lua: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire ?
Lire un fichier texte en programmation signifie ouvrir un fichier contenant du texte et en extraire son contenu pour l'utiliser dans votre code. Les programmeurs le font souvent pour lire des données externes ou pour stocker des informations dans un format facilement accessible.

# Comment faire :
Dans Lua, vous pouvez utiliser la fonction "io.open()" pour ouvrir un fichier texte et stocker son contenu dans une variable. Ensuite, vous pouvez utiliser la méthode "read()" pour lire le contenu du fichier. Voici un exemple de code :

```Lua
-- Ouvrir le fichier texte "monfichier.txt" en mode lecture
local fichier = io.open("monfichier.txt", "r")

-- Stocker le contenu du fichier dans une variable
local contenu = fichier:read()

-- Fermer le fichier
fichier:close()

-- Afficher le contenu du fichier
print(contenu)
```

Si le contenu du fichier est "Bonjour le monde !", la sortie sera "Bonjour le monde !". Vous pouvez également utiliser la méthode "lines()" pour lire le fichier ligne par ligne, ou "read("*all")" pour lire tout le contenu du fichier en une seule fois.

# Plongée en profondeur :
La fonction "io.open()" provient du module standard "io" de Lua, qui fournit des fonctions pour gérer les entrées et sorties. Avant Lua 5.2, la fonction "file:read()" était utilisée pour lire le contenu des fichiers, mais elle a été remplacée par un objet de fichier avec différentes méthodes, telles que "file:lines()" et "file:read()". Alternativement, vous pouvez également utiliser le module "lfs" pour lire des fichiers avec des fonctionnalités supplémentaires telles que la possibilité de spécifier le mode d'ouverture.

# Voir aussi :
Pour en savoir plus sur la gestion des fichiers en Lua, vous pouvez consulter la documentation officielle sur le module "io" et le module "lfs". Vous pouvez également trouver des exemples pratiques sur des blogs et des tutoriels en ligne. Bonne lecture !