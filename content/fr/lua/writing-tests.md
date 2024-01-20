---
title:                "Rédaction de tests"
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire des tests consiste à vérifier que votre code fonctionne comme prévu. Les programmeurs testent pour prévenir les bugs, garantir la qualité et faciliter les mises à jour.

## How to:
```Lua
-- Example: Simple Lua test using assert
function add(a, b)
    return a + b
end

-- Test
local result = add(2, 3)
assert(result == 5, "Expected 2 + 3 to equal 5")

print("Test passed: 2 + 3 equals " .. result)
```
Output:
```
Test passed: 2 + 3 equals 5
```

## Deep Dive
Historiquement, les tests en Lua pouvaient s'appuyer sur des frameworks tels que LuaUnit ou Busted. Les alternatives incluent l'écriture de vérifications manuelles avec `assert` ou l'utilisation de librairies tierces. L'implémentation dépend des besoins spécifiques, de la complexité du projet et des préférences en matière de rapport de test.

## See Also
- LuaUnit documentation: [https://luaunit.readthedocs.io/en/latest/](https://luaunit.readthedocs.io/en/latest/)
- Lua assertion manual: [https://www.lua.org/manual/5.4/manual.html#pdf-assert](https://www.lua.org/manual/5.4/manual.html#pdf-assert)