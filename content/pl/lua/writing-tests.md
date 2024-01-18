---
title:                "Pisanie testów"
html_title:           "Lua: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/writing-tests.md"
---

{{< edit_this_page >}}

## Co & dlaczego?

Pisanie testów jest procesem weryfikującym poprawność działania kodu. Programiści to wykonują, aby upewnić się, że zmiany w kodzie nie powodują błędów i spełniają oczekiwania. Jest to również częścią praktyki programistycznej, która pomaga utrzymać dobrą jakość kodu.

## Jak to zrobić:

```Lua
-- Przykładowa funkcja, którą będziemy testować. 
function dodaj(x, y)
    return x + y
end

-- Importowanie biblioteki do wykonywania testów. 
local test = require("luaunit")

-- Przykład prostego testu jednostkowego. 
function test_dodaj()
  assertEqual(5, dodaj(2, 3)) -- Sprawdza, czy wynik funkcji jest równy 5. 
end

-- Uruchamianie testów. 
test.run()
```

Output: 
```
.PLG
OK  	1 testDzienniczek Test: dodaj
OK  	Testsuite: test
Tests	: 1
Failures: 0
Errors	: 0
```

## Głębszy wgląd:

Pisanie testów zyskało na popularności wraz z rozwojem metodyki Agile. Pozwala ono na szybkie wykrywanie błędów i poprawianie ich na bieżąco, co wpływa na szybszy rozwój oprogramowania. Alternatywą dla pisania testów jest manualne testowanie kodu, jednak jest to czasochłonne i mniej skuteczne. W Lua dostępnych jest kilka bibliotek do testowania, ale najpopularniejszą jest LuaUnit.

## Zobacz również:

- Dokumentacja LuaUnit: https://github.com/luaunit/luaunit 
- Praktyka TDD w Lua: https://www.youtube.com/watch?v=hTfcUNlIVa0 
- Artykuł o testowaniu w języku Lua: https://medium.com/learning-the-go-programming-language/introduction-to-testing-in-go-151a70b2ddb1