---
title:                "Użycie Wielkich Liter w Ciągu Znaków"
html_title:           "Lua: Użycie Wielkich Liter w Ciągu Znaków"
simple_title:         "Użycie Wielkich Liter w Ciągu Znaków"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Capitalizing (kapitalizacja) oznacza zamianę pierwszej litery w ciągu znaków na wielką. Programiści często stosują tę technikę, gdy potrzebują, aby nazwa lub tekst był czytelny i wyraźniejszy.

## Jak to zrobić:
W Lua, możliwe jest wykorzystanie funkcji ```string.sub()``` oraz ```string.upper()``` do kapitalizacji tekstu. Poniżej przedstawione są przykładowe kody i wyniki:

```Lua
local string1 = "hello world"
local capitalized_string1 = string.upper(string1:sub(1,1)) .. string1:sub(2)
```
Wynik:
```Lua
Hello world
```
```Lua
local string2 = "programowanie jest fajne"
local capitalized_string2 = string2:gsub("^%l", string.upper)
```
Wynik:
```Lua
Programowanie jest fajne
```

## Głębsze zagadnienia:
Kapitalizacja tekstu jest przydatną funkcją, szczególnie przy tworzeniu interfejsów użytkownika lub analizie danych. Wcześniej, przede wszystkim w językach programowania typu C, kapitalizacja wymagała użycia specjalnych bibliotek lub stworzenia własnych funkcji. W Lua udostępniane są wbudowane funkcje, które ułatwiają ten proces.

## Zobacz również:
1. [Dokumentacja Lua](https://www.lua.org/docs.html)
2. [Inne przydatne funkcje tekstu w Lua](https://www.tutorialspoint.com/lua/lua_strings.htm)
3. [Źródło: Flavio Copes](https://flaviocopes.com/lua-string-uppercase/)