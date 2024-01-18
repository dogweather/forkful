---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "Lua: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?

Zamiana tekstu to proces, w którym szukamy określonego fragmentu tekstu i zastępujemy go innym. Programiści często wykonują tę operację, ponieważ pozwala to na szybkie i efektywne zmiany w kodzie, bez konieczności ręcznego przeszukiwania i edycji każdego wystąpienia danego tekstu.

## Jak to zrobić:

```Lua
-- Przykładowy tekst
local tekst = "Witaj, świecie!"

-- Zamiana tekstu "Witaj" na "Cześć"
local nowy_tekst = string.gsub(tekst, "Witaj", "Cześć")

print(nowy_tekst) -- Output: Cześć, świecie!
```

```Lua
-- Przykładowa tablica z danymi
local liczby = {1, 2, 3, 4}

-- Zastąpienie wartości 3 na 99
for i, liczba in ipairs(liczby) do
    if liczba == 3 then
        liczby[i] = 99
    end
end

print(table.concat(liczby, ", ")) -- Output: 1, 2, 99, 4
```

## Deep Dive:

Zamiana tekstu jest powszechnie stosowanym narzędziem w programowaniu. Pierwsze programy, które wprowadziły tę funkcję, powstały w latach 60. Wiele języków programowania, takich jak C czy Python, posiada wbudowane funkcje do wyszukiwania i zamiany tekstu.

Alternatywnym sposobem na zamianę tekstu jest wykorzystanie wyrażeń regularnych, co pozwala na bardziej zaawansowane wyszukiwanie i manipulację tekstem. W języku Lua, działanie takie jest osiągalne dzięki wykorzystaniu modułu [lpeg](https://www.inf.puc-rio.br/~roberto/lpeg/).

Implementacja funkcji zamiany tekstu w języku Lua jest oparta na metodzie "replace" z języka [Perl](https://www.perl.org/), a także na funkcji "gsub" z języka [awk](https://www.gnu.org/software/gawk/).

## Zobacz również:

- [Dokumentacja języka Lua](https://www.lua.org/docs.html)
- [Przewodnik po wyrażeniach regularnych w Lua](http://lua-users.org/wiki/LuaPatterns)