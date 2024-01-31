---
title:                "Tests schreiben"
date:                  2024-01-19
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Testen schreiben geht es darum, Code zu überprüfen, indem man automatisierte Skripte benutzt, die durchspielen, was der Code tun soll. Wir machen das, um Fehler frühzeitig zu erkennen und die Softwarequalität langfristig zu sichern.

## How to:
Mit Lua kannst du einfache Tests schreiben, indem du eine Testfunktion definierst und `assert` verwendest, um Bedingungen zu prüfen.

```Lua
-- Ein einfaches Testbeispiel
local function add(a, b)
    return a + b
end

-- Testfunktion
local function testAdd()
    assert(add(1, 2) == 3)
    assert(add(-1, -2) == -3)
    assert(add(0, 0) == 0)
    print("Alle Tests erfolgreich durchgeführt!")
end

-- Tests ausführen
testAdd()
```

Sample Output:
```
Alle Tests erfolgreich durchgeführt!
```

## Deep Dive
Die Praxis des Testens in der Programmierung existiert seit den frühen Tagen der Softwareentwicklung. In Lua sind Bibliotheken wie `busted` oder `luassert` beliebt, um umfangreichere Tests wie Unit-Tests oder Behavior-Driven Development (BDD) zu unterstützen. Diese Tools bieten mehr Funktionalitäten als einfache `assert`-Aufrufe, darunter Test-Suites, Mock-Objekte, und detaillierte Ausgabe.

## See Also
Weiterführende Ressourcen und Links:

- Lua Users Wiki zu automatisierten Tests: http://lua-users.org/wiki/UnitTesting
- busted Test-Framework: http://olivinelabs.com/busted/
- Einführung in die Testgesteuerte Entwicklung (TDD) mit Lua: https://www.lua.org/pil/8.html
