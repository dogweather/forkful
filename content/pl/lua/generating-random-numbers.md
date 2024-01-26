---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:49:30.298664-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Generowanie losowych liczb to sposób na tworzenie nieprzewidywalnych wartości. Programiści używają ich w grach, symulacjach czy przy testowaniu aplikacji, by wprowadzić element zaskoczenia i realizmu.

## How to: (Jak to zrobić:)
```Lua
-- Inicjalizacja generatora liczb losowych
math.randomseed(os.time())

-- Liczba całkowita z zakresu od 1 do 100
local randomInt = math.random(1,100)
print("Losowa liczba całkowita: " .. randomInt)

-- Liczba zmiennoprzecinkowa z zakresu od 0 do 1
local randomFloat = math.random()
print("Losowa liczba zmiennoprzecinkowa: " .. randomFloat)
```
Output:
```
Losowa liczba całkowita: 47
Losowa liczba zmiennoprzecinkowa: 0.013452918222375
```

## Deep Dive (Zagłębiamy się)
Losowość w programowaniu bywa trudna, bo komputery działają według określonych algorytmów. W Lua używamy `math.randomseed()` by zainicjować generator liczb pseudolosowych (RNG). Czas systemowy `os.time()` często jest używany jako ziarno (seed), dające inny punkt startowy przy każdym uruchomieniu programu. Alternatywą jest używanie zewnętrznych urządzeń lub zdarzeń do generowania prawdziwej losowości. 

Implementacja RNG w Lua opiera się na algorytmie Mersenne Twister, który jest jednym z bardziej popularnych ze względu na swoje dobre właściwości statystyczne i okres o dużej długości. Pamiętaj, że `math.random()` bez argumentów daje zmiennoprzecinkowe liczby pseudolosowe od 0 do 1, ale możesz przekazać argumenty, by uzyskać liczby całkowite z określonego zakresu.

## See Also (Zobacz także)
- Lua 5.4 reference manual: https://www.lua.org/manual/5.4/ (English)
- Tutorial o generowaniu liczb losowych w Lua: https://www.tutorialspoint.com/lua/lua_random_numbers.htm (English)
- Opis algorytmu Mersenne Twister: https://en.wikipedia.org/wiki/Mersenne_Twister (English)
