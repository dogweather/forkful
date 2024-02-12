---
title:                "Tests Schreiben"
aliases:
- /de/lua/writing-tests/
date:                  2024-02-03T19:31:12.138218-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schreiben"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben von Tests in der Programmierung bedeutet, kleine, separate Codeteile zu erstellen, um automatisch zu überprüfen, ob verschiedene Teile Ihrer Anwendung wie erwartet funktionieren. Für Lua-Programmierer sichert das Testen Zuverlässigkeit und hilft dabei, die Codequalität zu erhalten, beschleunigt den Debugging-Prozess und macht Änderungen am Code sicherer.

## Wie:

Lua, als leichtgewichtige, aber leistungsstarke Skriptsprache, beinhaltet kein eingebautes Test-Framework. Dritt-Paketbibliotheken wie Busted und LuaUnit machen das Testen jedoch relativ unkompliziert. Hier sehen wir uns Beispiele mit beiden an.

### Busted verwenden

Busted ist ein beliebtes Lua-Test-Framework, das eine flexible Art bietet, Tests zu schreiben. Installieren Sie zuerst Busted über LuaRocks (den Paketmanager von Lua) mit `luarocks install busted`. Nach der Installation können Sie Ihre Tests schreiben. Hier ist ein einfacher Test für eine Funktion `add`, die zwei Zahlen addiert:

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

describe("Add function", function()
  it("sollte zwei Zahlen korrekt addieren", function()
    assert.are.equal(5, add(2, 3))
  end)
end)
```

Um die Tests auszuführen, führen Sie `busted` in Ihrem Terminal aus. Eine Beispiel-Ausgabe für einen bestandenen Test könnte so aussehen:

```
●
1 Erfolg / 0 Misserfolge / 0 Fehler / 0 ausstehend : 0.002 Sekunden
```

### LuaUnit verwenden

LuaUnit ist ein weiteres Test-Framework, das den xUnit-Konventionen folgt und einfach einzurichten ist. Installieren Sie LuaUnit über LuaRocks mit `luarocks install luaunit`. So könnten Sie einen ähnlichen Test wie oben mit LuaUnit schreiben:

```lua
-- add.lua bleibt gleich

-- test_add.lua
luaunit = require('luaunit')
local add = require('add')

function testAdd()
  luaunit.assertEquals(add(2, 3), 5)
end

os.exit(luaunit.LuaUnit.run())
```

Wenn Sie dieses Skript direkt über Lua ausführen (`lua test_add.lua`), erhalten Sie etwas Ähnliches wie:

```
.
Ran 1 tests in 0.001 Sekunden, 1 Erfolg, 0 Misserfolge
```

Sowohl Busted als auch LuaUnit bieten umfangreiche Funktionen, um verschiedene Testszenarien zu behandeln, einschließlich Mocking, Spying und asynchronem Testen. Die Wahl zwischen ihnen hängt von den spezifischen Bedürfnissen Ihres Projekts und Ihren persönlichen Vorlieben bezüglich Syntax und Funktionalität ab.
