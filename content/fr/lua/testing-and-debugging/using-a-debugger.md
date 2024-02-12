---
title:                "Utilisation d'un débogueur"
aliases:
- /fr/lua/using-a-debugger/
date:                  2024-01-26T03:50:08.143505-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'un débogueur"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/using-a-debugger.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Un débogueur est un outil qui vous permet d'inspecter et de contrôler l'exécution d'un programme, facilitant ainsi la détection des problèmes. Les programmeurs utilisent des débogueurs pour éliminer les bugs, comprendre le flux du code et s'assurer que leur code est impeccable.

## Comment faire :
Lua n'est pas fourni avec un débogueur intégré, mais vous pouvez utiliser des débogueurs externes, comme ZeroBrane Studio. Voici un aperçu de la façon dont vous travailleriez avec :

```Lua
-- Ceci est un simple script Lua avec une erreur intentionnelle
local function add(a, b)
    local result = a + b -- Oups, faisons comme si nous avions oublié de définir 'b'
    return result
end

print(add(10))
```

Lorsque vous exécutez cela dans un débogueur, il arrêtera l'exécution là où les choses se gâtent. Vous verrez quelque chose comme ceci :

```
lua: example.lua:3: tentative de réaliser une opération arithmétique sur une valeur nil (local 'b')
trace de la pile :
	example.lua:3: dans la fonction 'add'
	example.lua:7: dans le bloc principal
	[C]: dans ?
```

Vous pouvez placer des points d'arrêt, parcourir votre code pas à pas et examiner les valeurs des variables pour traquer le bug sans perdre la tête.

## Plongée profonde
La simplicité de Lua ne s'étend malheureusement pas au débogage. Pas de soucis cependant, la communauté Lua vous soutient. Des outils comme ZeroBrane Studio, LuaDec et d'autres offrent des capacités de débogage. Historiquement, les débogueurs existaient peu de temps après que les premiers programmes ont tourné au vinaigre, donnant aux développeurs le moyen de corriger leur code sans tâtonner à l'aveugle.

Avec Lua, vous comptez souvent sur des débogueurs externes ou les intégrez à votre environnement de développement. ZeroBrane Studio, par exemple, est un IDE qui intègre pleinement un débogueur Lua. Il vous permet de parcourir le code pas à pas, de placer des points d'arrêt et de surveiller les variables. Du côté de l'implémentation, les débogueurs utilisent généralement des crochets pour insérer des points d'arrêt et d'autres installations de débogage.

Des alternatives ? Bien sûr. De bons vieux messages `print`, affectueusement connus sous le nom de débogage "printf", peuvent parfois faire l'affaire sans outils sophistiqués.

## Voir aussi
Pour poursuivre votre voyage de débogage, consultez :

- ZeroBrane Studio : https://studio.zerobrane.com/
- Wiki des utilisateurs de Lua sur le débogage du code Lua : http://lua-users.org/wiki/DebuggingLuaCode
- La référence de la bibliothèque `debug` dans le manuel de Lua : https://www.lua.org/manual/5.4/manual.html#6.10
