---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
L'Impression de sortie de débogage en Lua est un moyen simple de vérifier l’état du code à différents points en cours d'exécution. Les programmeurs l'utilisent pour repérer et résoudre les problèmes de manière efficace.

## Comment faire:
Voici comment imprimer une sortie de débogage en Lua:
```Lua
--Définir une variable
debug = "Hello, Debug!"

--Imprimer la variable
print(debug)
```
L'exécution du code ci-dessus donnera la sortie suivante:
```Lua
Hello, Debug!
```
## Plongée Profonde
L'impression de sortie de débogage en Lua, bien que simple, est essentielle pour le débogage. Historiquement, ce mécanisme remonte aux débuts de la programmation. Il existe également d'autres alternatives comme l'utilisation de débogueurs de niveau professionnel (comme ZeroBrane) qui offrent de meilleures fonctions de débogage. Cependant, `print()` reste un choix rapide et facile pour les vérifications simples.

En parlant de la mise en œuvre, la fonction `print()` en Lua convertit tous les arguments en chaînes de caractères et les envoie à la sortie standard. Elle est souvent utilisée dans vos scripts Lua pour afficher des messages de débogage de manière simple et rapide.

## Voir Aussi
1. [Guide Lua pour les débutants](https://learnxinyminutes.com/docs/fr-fr/lua-fr/)
2. [Documentation Lua 5.2](http://www.lua.org/manual/5.2/manual.html)
3. [ZeroBrane - Un environnement de développement Lua](https://studio.zerobrane.com/docLua-debugging-guide)
4. [Apprendre à coder en Lua](https://www.tutorialspoint.com/lua/index.htm)