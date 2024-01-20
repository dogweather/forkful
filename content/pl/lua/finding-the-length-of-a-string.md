---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Znalezienie długości ciągu to proces określania ilości znaków w tym ciągu. Programiści robią to, aby kontrolować i manipulować danymi tekstowymi w ich kodzie.

## Jak to zrobić:
Znalezienie długości ciągu w Lua jest proste. Użyj wbudowanej funkcji `string.len`.

Oto przykład:

```Lua
local moj_ciąg = "Cześć, świecie!"
print("Długość mojego ciągu to: " .. string.len(moj_ciąg))
```

Na wyjściu zobaczysz:

```Lua
Długość mojego ciągu to: 16
```

## Głębsze spojrzenie
Funkcję `string.len` wprowadzono w Lua 5.1 i jest ona od tego czasu podstawą języka. Istnieją inne metody znalezienia długości ciągu, np `#` operator.

```Lua
local moj_ciąg = "Cześć, świecie!"
print("Długość mojego ciągu to: " .. #moj_ciąg)
```

Ta metoda jest znacznie bardziej skrótowa, ale zachowuje tę samą funkcję. W kontekście implementacji, `string.len` i `#` to po prostu aliasy tej samej funkcji języka C.

## Zobacz także
- Dokumentacja Lua 5.1: https://www.lua.org/manual/5.1/
- Więcej na temat operatora `#` : https://www.tutorialspoint.com/lua/lua_operators.htm
- Więcej na temat zarządzania tekstem w Lua: https://www.tutorialspoint.com/lua/lua_strings.htm