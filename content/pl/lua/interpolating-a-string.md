---
title:                "Interpolacja łańcuchów znaków"
date:                  2024-01-20T17:51:42.804314-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"

category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Interpolacja łańcuchów pozwala wpleść wartości zmiennych do tekstu. Programiści używają jej dla czytelności i wygody, eliminując potrzebę ciągłego łączenia ciągów za pomocą konkatenacji.

## How to: (Jak to zrobić:)
Lua nie ma wbudowanej funkcji interpolacji łańcuchów, ale można osiągnąć podobny efekt wykorzystując `string.format` lub grawisów (w wersji 5.3+).

```Lua
-- Użycie string.format
local name = "Łukasz"
local age = 28
local greeting = string.format("Cześć, nazywam się %s i mam %d lat.", name, age)
print(greeting)  -- Cześć, nazywam się Łukasz i mam 28 lat.

-- Użycie grawisów
local temperature = 23.5
local weather = "Temperatura dzisiaj to: `${tempC}°C`."
weather = weather:gsub('`', ''):gsub("%${(.-)}", {tempC = temperature})
print(weather)  -- Temperatura dzisiaj to: 23.5°C.
```

## Deep Dive (Dogłębna analiza):
Lua nie ma wbudowanej interpolacji łańcuchów jak w niektórych innych językach (np. Ruby czy Python). Musimy używać funkcji `string.format`, która działa podobnie do printf w C, lub pisać własne rozwiązania.

Wersja 5.3+ wprowadziła mechanizm do obsługi UTF-8, co umożliwia lepszą manipulację polskimi znakami w stringach. Warto zdawać sobie sprawę, że każde niestandardowe rozwiązanie może wprowadzać dodatkowe złożoności, jak różnicowanie wydajności czy czytelności.

Alternatywnie, możemy zaimplementować własną funkcję interpolującą, używając `gsub` do zamiany określonych wzorców w tekście przez wartości z tabeli.

## See Also (Zobacz również):
- Oficjalna dokumentacja Lua 'string' library: https://www.lua.org/manual/5.4/manual.html#6.4
- Informacje o UTF-8 w Lua: https://www.lua.org/manual/5.3/manual.html#6.5
- Wprowadzenie do Lua: https://www.lua.org/pil/
