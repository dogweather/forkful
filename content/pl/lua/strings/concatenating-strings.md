---
date: 2024-01-20 17:35:17.283503-07:00
description: "How to: (\"Jak to zrobi\u0107:\") \u0141\u0105czenie napis\xF3w jest\
  \ fundamentem wi\u0119kszo\u015Bci j\u0119zyk\xF3w programowania i bywa cz\u0119\
  sto wykorzystywane w Lua. Historycznie, operacje\u2026"
lastmod: '2024-04-05T22:50:49.854214-06:00'
model: gpt-4-1106-preview
summary: "(\"Jak to zrobi\u0107:\") \u0141\u0105czenie napis\xF3w jest fundamentem\
  \ wi\u0119kszo\u015Bci j\u0119zyk\xF3w programowania i bywa cz\u0119sto wykorzystywane\
  \ w Lua."
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 3
---

## How to: ("Jak to zrobić:")
```Lua
-- Łączenie za pomocą operatora ..
local napis1 = "Dzień"
local napis2 = " dobry!"
local przywitania = napis1 .. napis2
print(przywitania) -- Wyświetla "Dzień dobry!"

-- Łączenie z użyciem funkcji table.concat
local slowa = {"Hej", "świecie", "to", "Lua!"}
local zdanie = table.concat(slowa, " ")
print(zdanie) -- Wyświetla "Hej świecie to Lua!"

-- Formatowanie z użyciem string.format
local imie = "Anna"
local powitanie = string.format("Cześć, %s!", imie)
print(powitanie) -- Wyświetla "Cześć, Anna!"
```

## Deep Dive ("Dogłębna analiza")
Łączenie napisów jest fundamentem większości języków programowania i bywa często wykorzystywane w Lua. Historycznie, operacje na napisach bywały wolne, dlatego wydajność była często ważna. W Lua operator `..` jest szybki, ale przy łączeniu wielu napisów lepszym wyborem może być `table.concat`, z uwagi na niższą złożoność czasową. `string.format` natomiast jest idealny do skomplikowanego formatowania i dodatkowo korzystny, gdy chcemy dbać o czytelność.

Alternatywą dla operatora `..` mogą być bufory napisów lub funkcje biblioteki `string`. Bufory napisów są użyteczne, gdy pracujemy z bardzo dużymi napisami lub w sytuacji, gdy wydajność jest szczególnie krytyczna, np. w grach komputerowych.

Szczegóły implementacji w języku Lua zależą od wersji. Lua 5.3 i nowsze używają techniki zwanej "copy-on-write" aby zoptymalizować wydajność operacji na napisach, zmniejszając konieczność kopiowania napisów przy każdym łączeniu.

## See Also ("Zobacz także")
- Lua 5.4 Reference Manual (Manual odniesienia Lua 5.4) - [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
- Programming in Lua (Programowanie w Lua) - [https://www.lua.org/pil/](https://www.lua.org/pil/)
