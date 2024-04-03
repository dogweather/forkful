---
date: 2024-01-20 17:51:42.804314-07:00
description: "Interpolacja \u0142a\u0144cuch\xF3w pozwala wple\u015B\u0107 warto\u015B\
  ci zmiennych do tekstu. Programi\u015Bci u\u017Cywaj\u0105 jej dla czytelno\u015B\
  ci i wygody, eliminuj\u0105c potrzeb\u0119 ci\u0105g\u0142ego\u2026"
lastmod: '2024-03-13T22:44:35.525882-06:00'
model: gpt-4-1106-preview
summary: "Interpolacja \u0142a\u0144cuch\xF3w pozwala wple\u015B\u0107 warto\u015B\
  ci zmiennych do tekstu."
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 8
---

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
