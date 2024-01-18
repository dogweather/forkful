---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Lua: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Znajdywanie długości ciągu znaków to często spotykana czynność w programowaniu. Polega ona na obliczeniu liczby znaków, które zawiera dany ciąg tekstu. Programiści często potrzebują tej informacji, ponieważ pozwala ona na wykonywanie różnych operacji na ciągach znaków, takich jak sprawdzanie, czy dany ciąg jest pusty czy też przycinanie zbyt długich ciągów.

## Jak to zrobić:

```Lua
local string = "Hello world!"
print(#string)
```
**Wynik:** 12

Możemy również użyć funkcji `string.len()` aby uzyskać tę samą informację.

```Lua
local string = "Hello world!"
print(string.len(string))
```
**Wynik:** 12

## Wnikliwe spojrzenie:

Obliczanie długości ciągu znaków jest powszechnie używanym narzędziem w programowaniu. Wcześniej, w językach takich jak C lub Java, konieczne było ręczne iterowanie po ciągu znaków aby obliczyć jego długość. W Lua natomiast, wystarczy użyć operatora `#` lub funkcji `string.len()`.

Alternatywną metodą jest używanie funkcji `string.gmatch()` do znalezienia liczby wystąpień danego znaku w ciągu. Jednakże, ta metoda jest mniej efektywna i wymaga większej ilości kodu.

Kilka funkcji biblioteki standardowej Lua odwołuje się do długości ciągu znaków, takich jak `string.sub()`, `string.find()`, czy `string.format()`. Dzięki temu, znajdując długość ciągu, jesteśmy w stanie lepiej wykorzystać możliwości tych funkcji.

## Zobacz też:

- [Oficjalna dokumentacja Lua o operacji obliczania długości ciągu znaków](https://www.lua.org/manual/5.3/manual.html#3.4.7)
- [Artykuł o długości ciągu znaków na stronie "Lua Gems"](https://luagems.org/lua-string-length/)