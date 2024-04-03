---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:31.319974-07:00
description: "Pisanie test\xF3w w programowaniu polega na tworzeniu ma\u0142ych, oddzielnych\
  \ kawa\u0142k\xF3w kodu, kt\xF3re automatycznie weryfikuj\u0105, czy r\xF3\u017C\
  ne cz\u0119\u015Bci aplikacji dzia\u0142aj\u0105\u2026"
lastmod: '2024-03-13T22:44:35.543470-06:00'
model: gpt-4-0125-preview
summary: "Pisanie test\xF3w w programowaniu polega na tworzeniu ma\u0142ych, oddzielnych\
  \ kawa\u0142k\xF3w kodu, kt\xF3re automatycznie weryfikuj\u0105, czy r\xF3\u017C\
  ne cz\u0119\u015Bci aplikacji dzia\u0142aj\u0105 zgodnie z oczekiwaniami."
title: "Pisanie test\xF3w"
weight: 36
---

## Jak to zrobić:
Lua, będąc lekkim, lecz potężnym językiem skryptowym, nie zawiera wbudowanego frameworka do testowania. Jednakże biblioteki stron trzecich, takie jak Busted i LuaUnit, sprawiają, że testowanie jest stosunkowo proste. Tutaj przyjrzymy się przykładom z użyciem obu.

### Korzystanie z Busted
Busted to popularny framework do testowania Lua, który oferuje elastyczny sposób na pisanie testów. Po pierwsze, zainstaluj Busted przez LuaRocks (menedżer pakietów Lua) z `luarocks install busted`. Po instalacji możesz pisać swoje testy. Oto prosty test dla funkcji `add`, która sumuje dwie liczby:

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

describe("Funkcja dodawania", function()
  it("powinna prawidłowo dodawać dwie liczby", function()
    assert.are.equal(5, add(2, 3))
  end)
end)
```

Aby uruchomić testy, wykonaj `busted` w terminalu. Przykładowe wyjście dla udanego testu wyglądałoby tak:

```
●
1 sukces / 0 porażek / 0 błędów / 0 oczekujących : 0.002 sekundy
```

### Korzystanie z LuaUnit
LuaUnit to kolejny framework do testowania, który podąża za konwencjami xUnit i jest łatwy w konfiguracji. Zainstaluj LuaUnit przez LuaRocks używając `luarocks install luaunit`. Oto jak można napisać podobny test jak wyżej z LuaUnit:

```lua
-- add.lua pozostaje bez zmian

-- test_add.lua
luaunit = require('luaunit')
local add = require('add')

function testAdd()
  luaunit.assertEquals(add(2, 3), 5)
end

os.exit(luaunit.LuaUnit.run())
```

Uruchomienie tego skryptu bezpośrednio za pomocą Lua (`lua test_add.lua`) wyświetli coś takiego:

```
.
Uruchomiono 1 test w 0.001 sekundy, 1 sukces, 0 porażek
```

Zarówno Busted, jak i LuaUnit oferują obszerne funkcje do radzenia sobie z różnymi scenariuszami testowymi, w tym mokowanie, szpiegowanie i testowanie asynchroniczne. Wybór między nimi zależy od konkretnych potrzeb projektu i osobistych preferencji dotyczących składni i funkcjonalności.
