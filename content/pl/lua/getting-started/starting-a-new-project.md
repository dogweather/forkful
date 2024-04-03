---
date: 2024-01-20 18:04:36.633085-07:00
description: "How to (Jak to zrobi\u0107): Tworzenie nowego projektu w Lua jest proste.\
  \ Za\u0142\xF3\u017Cmy, \u017Ce nasz projekt to gra \"Zgadnij Liczb\u0119\". Zacznij\
  \ od podstawowego pliku\u2026"
lastmod: '2024-03-13T22:44:35.540670-06:00'
model: gpt-4-1106-preview
summary: Tworzenie nowego projektu w Lua jest proste.
title: Rozpoczynanie nowego projektu
weight: 1
---

## How to (Jak to zrobić):
Tworzenie nowego projektu w Lua jest proste. Załóżmy, że nasz projekt to gra "Zgadnij Liczbę". Zacznij od podstawowego pliku `main.lua`.

```Lua
-- main.lua
math.randomseed(os.time()) -- inicjalizacja generatora liczb losowych

local secretNumber = math.random(1, 100) -- losowa liczba do zgadnięcia
print("Zgadnij liczbę od 1 do 100.")

while true do
    print("Wpisz swoją liczbę:")
    local guess = tonumber(io.read())
    
    if guess == secretNumber then
        print("Zgadłeś! Tajemnicza liczba to " .. secretNumber .. ".")
        break
    elseif guess < secretNumber then
        print("Za mało!")
    else
        print("Za dużo!")
    end
end
```

Przykładowy wynik działania programu:
```
Zgadnij liczbę od 1 do 100.
Wpisz swoją liczbę:
> 50
Za mało!
Wpisz swoją liczbę:
> 75
Za dużo!
Wpisz swoją liczbę:
> 62
Zgadłeś! Tajemnicza liczba to 62.
```

## Deep Dive (Głębsze Zanurzenie):
Lua powstała w 1993 roku w Brazylii. Ceniona za prostotę, wydajność i elastyczność, Lua używana jest często w skryptowaniu, grach i systemach wbudowanych. Alternatywami dla Lua może być Python dla prostych skryptów lub C++ dla wydajnościowych systemów wbudowanych. Ważne przy rozpoczęciu projektu w Lua jest zrozumienie jak działa zarządzanie pamięcią i tabele, które są podstawowym typem danych w tym języku.

## See Also (Zobacz Również):
- Oficjalna strona Lua: [https://www.lua.org](https://www.lua.org)
- Dokumentacja Lua 5.4 (aktualna wersja): [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
