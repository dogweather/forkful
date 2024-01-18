---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Lua: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce quec'est que la lecture des arguments de ligne de commande? 
La lecture des arguments de ligne de commande est le fait de récupérer les valeurs saisies par l'utilisateur dans le terminal lorsqu'il exécute votre code Lua en ligne de commande. Les programmeurs utilisent cette méthode pour rendre leurs programmes plus flexibles et personnalisables, car elle permet aux utilisateurs de fournir des informations spécifiques au moment de l'exécution.

## Comment faire: 
Dans Lua, il existe une variable globale appelée "arg" qui contient tous les arguments de ligne de commande sous forme de tableau. Voici un exemple de code pour afficher tous ces arguments sur le terminal:

```Lua
for i, v in ipairs(arg) do
  print(i, v)
end
```

Si vous souhaitez récupérer un argument spécifique, vous pouvez utiliser son index comme ceci:

```Lua
local argument = arg[1] -- récupère le premier argument
print("Mon argument est: ", argument)
```
Exemple de sortie:
```
1	love
2	hello
3	world
```

## Plongée en profondeur: 
Cette méthode de lecture des arguments de ligne de commande existe depuis les premières versions de Lua. Cependant, il existe également d'autres solutions telles que la bibliothèque "argparse" pour une gestion plus poussée des arguments. De plus, Lua permet également aux programmeurs de définir des options en utilisant le caractère "-" pour les séparer des arguments. Par exemple: `myprogram.lua -v arg1 arg2`, où "-v" serait l'option définie par le programmeur.

## A voir également: 
- Documentation officielle de Lua sur la variable "arg": https://www.lua.org/pil/12.1.html
- Documentation de la bibliothèque "argparse": https://github.com/mpeterv/argparse