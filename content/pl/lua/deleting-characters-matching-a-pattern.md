---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Usuwanie znaków pasujących do danego wzorca to proces, który polega na identyfikacji i usunięciu określonych ciągów znaków z większego tekstu. Programiści korzystają z tej metody, aby sprzątnąć dane wejściowe, usunąć niepotrzebne znaki lub przetworzyć tekst na bardziej pożądany format.

## Jak to zrobić:

W Lua, możemy użyć funkcji gsub(), aby usunąć znaki pasujące do wzorca. Oto proste przykłady.

```Lua
local str = "H3110 W0rld, Lua Pr0gramm1ng!"

-- Usuwanie cyfr
local result = str:gsub("%d", "")
print(result)  -- Output: "H World, Lua Programming!"

-- Usunięcie znaków nie będałcych literami
result = str:gsub("%W", "")
print(result)  -- Output: "H3110W0rldLuaPr0gramm1ng"
```

## Głębsze zrozumienie

Funkcja gsub() jest częścią biblioteki standardowej string w Lua, dostępnej od jego pierwszych wersji. Wspiera wzorce, które są trochę podobne do wyrażeń regularnych w Perl, ale mniej rozbudowane. 

Jeżeli chodzi o alternatywy, mogłoby to obejmować napisanie własnej funkcji do iteracji przez ciąg, ale gsub() jest na tyle wydajne i wszechstronne, że rzadko jest to konieczne. 

Co ciekawe, gsub() zwraca dwa wyniki: przetworzony ciąg i liczbę dokonanych zastąpień, co może być przydatne w niektórych przypadkach.

```Lua
local result, count = str:gsub("%d", "")
print(result, count)  -- Output: "H World, Lua Programming!" 6
```

## Zobacz także:

Przydatne źródła dotyczące tego tematu obejmują dokumentację Lua  
1. [String Manipulation in Lua](https://www.lua.org/manual/5.4/manual.html#6.4)
2. [Lua Patterns Tutorial](https://www.lua.org/pil/20.2.html)
3. [Lua gsub function](https://www.lua.org/manual/5.4/manual.html#pdf-string.gsub)

Jak zawsze, praktyka jest najlepszym nauczycielem, więc warto spróbować różnych wzorców i stringów, aby zrozumieć, jak to działa.