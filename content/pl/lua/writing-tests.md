---
title:                "Pisanie testów"
aliases:
- pl/lua/writing-tests.md
date:                  2024-02-03T19:31:31.319974-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie testów"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie testów w programowaniu polega na tworzeniu małych, oddzielnych kawałków kodu, które automatycznie weryfikują, czy różne części aplikacji działają zgodnie z oczekiwaniami. Dla programistów Lua, testowanie zapewnia niezawodność i pomaga w utrzymaniu jakości kodu, przyspiesza proces debugowania i czyni modyfikacje w bazie kodu bezpieczniejszymi.

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
