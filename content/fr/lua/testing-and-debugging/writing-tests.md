---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:16.093862-07:00
description: "\xC9crire des tests en programmation implique de cr\xE9er de petits\
  \ morceaux de code s\xE9par\xE9s pour v\xE9rifier automatiquement que diff\xE9rentes\
  \ parties de votre\u2026"
lastmod: '2024-03-13T22:44:57.940220-06:00'
model: gpt-4-0125-preview
summary: "\xC9crire des tests en programmation implique de cr\xE9er de petits morceaux\
  \ de code s\xE9par\xE9s pour v\xE9rifier automatiquement que diff\xE9rentes parties\
  \ de votre\u2026"
title: "R\xE9daction de tests"
weight: 36
---

## Quoi & Pourquoi ?

Écrire des tests en programmation implique de créer de petits morceaux de code séparés pour vérifier automatiquement que différentes parties de votre application fonctionnent comme prévu. Pour les programmeurs Lua, les tests garantissent la fiabilité et aident à maintenir la qualité du code, accélérant le processus de débogage et rendant les modifications de la base de code plus sûres.

## Comment faire :

Lua, étant un langage de script léger mais puissant, n'inclut pas de framework de test intégré. Cependant, des bibliothèques tierces comme Busted et LuaUnit rendent les tests relativement simples. Ici, nous examinerons des exemples utilisant les deux.

### Utiliser Busted

Busted est un framework de test Lua populaire qui offre une manière flexible d'écrire des tests. Tout d'abord, installez Busted via LuaRocks (le gestionnaire de paquets de Lua) avec `luarocks install busted`. Une fois installé, vous pouvez écrire vos tests. Voici un test simple pour une fonction `add` qui additionne deux nombres :

```lua
-- add.lua
local function add(a, b)
  return a + b
end

return add
```

```lua
-- add_spec.lua
local add = require('add')

describe("La fonction Add", function()
  it("devrait additionner correctement deux nombres", function()
    assert.are.equal(5, add(2, 3))
  end)
end)
```

Pour exécuter les tests, tapez `busted` dans votre terminal. Un exemple de sortie pour un test réussi ressemblerait à :

```
●
1 succès / 0 échecs / 0 erreurs / 0 en attente : 0,002 secondes
```

### Utiliser LuaUnit

LuaUnit est un autre framework de test qui suit les conventions xUnit et est facile à configurer. Installez LuaUnit via LuaRocks en utilisant `luarocks install luaunit`. Voici comment vous pourriez écrire un test similaire à celui ci-dessus avec LuaUnit :

```lua
-- add.lua reste le même

-- test_add.lua
luaunit = require('luaunit')
local add = require('add')

function testAdd()
  luaunit.assertEquals(add(2, 3), 5)
end

os.exit(luaunit.LuaUnit.run())
```

Exécuter ce script directement via Lua (`lua test_add.lua`) affichera quelque chose comme :

```
.
1 test lancé en 0,001 secondes, 1 succès, 0 échecs
```

Busted et LuaUnit offrent tous deux de vastes fonctionnalités pour gérer divers scénarios de test, incluant le mocking, l'espionnage et les tests asynchrones. Le choix entre eux repose sur les besoins spécifiques de votre projet et votre préférence personnelle en ce qui concerne la syntaxe et les fonctionnalités.
