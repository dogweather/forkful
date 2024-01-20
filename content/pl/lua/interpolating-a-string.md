---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Interpolacja łańcuchów to proces, który pozwala wstawiać zmienne i wyrażenia bezpośrednio w łańcuchy. Programiści to robią, aby pisać bardziej czytelny, zwięzły i elastyczny kod.

## Jak to zrobić:

Lua nie posiada wbudowanej funkcji do interpolacji łańcuchów. Możesz to jednak zrobić za pomocą funkcji `string.format`.

```Lua
name = "Jan"
greet = string.format("Cześć, %s", name)
print(greet) -- "Cześć, Jan"
```

Podobnie możemy robić operacje na zmiennych, które chcemy wstawić do łańcucha.

```Lua
x = 7
y = 8
msg = string.format("Suma %d i %d wynosi %d", x, y, x+y)
print(msg) -- "Suma 7 i 8 wynosi 15"
```

## Głębsze spojrzenie:

Interpolacja łańcuchów pochodzi z Pascala i została wprowadzona do większości języków programowania. W Pythonie i Ruby, interpolacja łańcuchów jest wbudowana, ale Lua wymaga ręcznego formatowania.

Alternatywą dla `string.format` jest stworzenie własnej funkcji, która wykonuje interpolację łańcuchów.

```Lua
function interp(s, tab)
    return (s:gsub('($%b{})', function(w) return tab[w:sub(3, -2)] end))
end

tab = { name = "Jan", age = 22 }
s = "Mam na imię ${name} i mam ${age} lata."
print(interp(s, tab)) -- "Mam na imię Jan i mam 22 lata."
```

Ta metoda korzysta z przekazywania tablicy do funkcji i wyszukuje odpowiednich wartości.

## Zobacz też:

1. [Tutorial Lua](http://tylerneylon.com/a/learn-lua/) - dla nauki podstaw Lua.
2. [Dokumentacja Lua](https://www.lua.org/manual/5.4/) - aby uzyskać więcej informacji o funkcji `string.format` i innych funkcjach Lua.