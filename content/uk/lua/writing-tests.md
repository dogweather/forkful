---
title:                "Написання тестів"
date:                  2024-01-19
simple_title:         "Написання тестів"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/writing-tests.md"
---

{{< edit_this_page >}}

## Що й Навіщо?
Тестування коду - це перевірка його на коректність та надійність. Програмісти пишуть тести для уникнення помилок та довготривалої стабільності програм.

## Як саме:
```Lua
-- Приклад простого модульного тесту в Lua
local function add(a, b)
    return a + b
end

-- Тест
local function testAdd()
    assert(add(2, 3) == 5)
    assert(add(-1, -1) == -2)
    assert(add(0, 0) == 0)
    print("testAdd пройдено")
end

-- Запускаємо тести
testAdd()
```
Output:
```
testAdd пройдено
```

## Поглиблено:
Тестування коду - практика стара як саме програмування. Альтернативами є TDD (Test Driven Development) та BDD (Behavior Driven Development). Lua не має вбудованої бібліотеки тестування, але є сторонні инструменти, як луаюніт (LuaUnit) или бастед (busted), які розширюють можливості тестування.

## Дивіться також:
- LuaUnit: https://luaunit.readthedocs.io/en/latest/
- busted: https://olivinelabs.com/busted/
- Стаття про TDD у Lua: http://lua-users.org/wiki/TestDrivenDevelopment
