---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Lire les arguments de la ligne de commande contrairement au Lua, c'est récupérer des valeurs externes lors du lancement de votre script. Pourquoi le faire? Simplicité, flexibilité, et interactivité, généralement pour modifier le comportement de votre programme.

## Comment faire:

En Lua, les arguments de l'invite sont stockés dans un tableau de script global nommé `arg`. Voyons cela en action:

```Lua
--fichier: showargs.lua
for i = 0, #arg do
    print('arg[' .. i .. ']= ' .. arg[i])
end
```

Exécutez-le avec des arguments, comme ça:

```bash
lua showargs.lua arg1 'second arg' arg3
```

Il affiche:

```bash
arg[0]= showargs.lua
arg[1]= arg1
arg[2]= second arg
arg[3]= arg3
```

## Plongée profonde

Historiquement, les arguments de ligne de commande datent du début de systèmes d'exploitation basés sur des textes, bien avant Lua. Comme alternatives à Lua, vous pouvez utiliser des bibliothèques spécialisées comme `lapp` ou `argparse`. Notez que Lua stocke ses arguments à partir de l'index 0, qui contient toujours le nom du script lui-même.

## Voir aussi

Pour comprendre plus de détails, consultez les ressources suivantes :

- Documentation Lua - [Programme en Ligne de Commande](https://www.lua.org/pil/25.html)
- Un article intéressant sur [l'utilisation des arguments en Lua](https://riptutorial.com/lua/example/2037/command-line-arguments)