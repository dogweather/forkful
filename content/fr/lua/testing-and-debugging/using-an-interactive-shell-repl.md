---
title:                "Utilisation d'une console interactive (REPL)"
date:                  2024-01-26T04:16:03.964951-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'une console interactive (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
REPL signifie Read-Eval-Print Loop (Boucle de Lecture-Évaluation-Affichage), un environnement interactif où vous pouvez rapidement tester du code. Les programmeurs l'utilisent pour expérimenter, déboguer et apprendre les particularités d'un langage.

## Comment faire :
Pour entrer dans le REPL de Lua, saisissez simplement `lua` dans votre terminal. Voici un exemple de session :

```Lua
> x = 10
> print(x * 2)
20
> t = {'pomme', 'banane', 'cerise'}
> table.insert(t, 'datte')
> for i, fruit in ipairs(t) do print(i, fruit) end
1	pomme
2	banane
3	cerise
4	datte
>
```
Dans la session, nous déclarons une variable, effectuons une arithmétique de base, manipulons un tableau, et parcourons ses éléments.

## Plongée profonde
La nature légère de Lua rend son REPL idéal pour le prototypage. Il existe depuis les débuts de Lua au début des années 1990, inspiré par les shells interactifs antérieurs pour des langages comme Lisp. Les alternatives dans d'autres langages incluent `irb` pour Ruby et `python` pour Python, chacun avec son propre ensemble de fonctionnalités. Le REPL de Lua est minimaliste ; de ce fait, il peut manquer de fonctionnalités avancées trouvées chez les autres, comme des outils de débogage complexes. Pour une expérience plus complète, des outils comme ZeroBrane Studio ou LuaDist's LuaRocks offrent plus que le REPL de base.

## Voir Aussi
- [Manuel de Référence de Lua 5.4 - L'interpréteur Lua autonome](https://www.lua.org/manual/5.4/manual.html#6) (lien en anglais)
- [ZeroBrane Studio](https://studio.zerobrane.com/) (lien en anglais)
- [LuaRocks](https://luarocks.org/) (lien en anglais)
