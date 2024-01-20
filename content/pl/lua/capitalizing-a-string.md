---
title:                "Zamiana liter na wielkie w łańcuchu znaków"
html_title:           "Lua: Zamiana liter na wielkie w łańcuchu znaków"
simple_title:         "Zamiana liter na wielkie w łańcuchu znaków"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zmiana wielkości liter w ciągu polega na zamianie początkowych liter każdego słowa na wielkie. Programiści robią to, aby poprawić czytelność tekstu lub spełnić specyficzne wymagania formatowania.

## Jak to zrobić:
Możemy to osiągnąć za pomocą wbudowanej funkcji `gsub` w Lua. 

```Lua
function Capitalize(str)
    return (str:gsub("^%l", string.upper))
end

print(Capitalize("witaj, świat"))  -- Wydrukuj wynik: "Witaj, świat"
```
W powyższym kodzie, `%l` odpowiada za znalezienie pierwszej małej litery słowa, a `string.upper` konwertuje ją na dużą literę.

## Głębsza analiza
Zmiana liter na wielkie w swoich ciągach jest praktyką, która istnieje od początków informatyki, kiedy to różne standardy wymagały różnych konwencji nazewnictwa. W różnych językach programowania, funkcje do manipulacji ciągami znaków, takie jak ta, często są zawarte w standardowych bibliotekach.

Jedną z alternatyw jest użycie funkcji `upper()`, jednak ta zmienia wszystkie litery na wielkie, a nie tylko pierwszą.

Szczegółowo, funkcja `gsub` to potężne narzędzie, które pozwala na wykonywanie złożonych operacji na ciągach za pomocą wyrażeń regularnych.

## Zobacz także
Jeśli chcesz dowiedzieć się więcej o manipulacji ciągami w Lua, polecam te źródła:

- Dokumentacja Lua: [Manipulacja ciągami](http://www.lua.org/manual/5.3/manual.html#6.4)
- Programowanie w Lua: [Ciągi](https://www.lua.org/pil/20.html)
- Lua-Users Wiki: [Tutorialy o ciągach](http://lua-users.org/wiki/StringRecipes)