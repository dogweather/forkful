---
title:                "Wykorzystanie wyrażeń regularnych"
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Regularne wyrażenia to wzorce służące do wyszukiwania i manipulowania tekstami. Programiści używają ich dla szybkiego przetwarzania ciągów znaków i automatyzacji zadań związanych z tekstem.

## Jak to zrobić:
```Lua
-- Wyszukiwanie pasującego wzorca
local tekst = "Hej, czy to jest twój numer 123-456-7890?"
local wzorzec = "%d%d%d-%d%d%d-%d%d%d%d"
print(string.match(tekst, wzorzec))

-- Wypisanie wyniku:
-- 123-456-7890

-- Zastępowanie tekstu pasującego do wzorca
local nowyTekst = string.gsub(tekst, wzorzec, "numer ukryty")
print(nowyTekst)

-- Wypisanie wyniku:
-- Hej, czy to jest twój numer ukryty?
```

## Głębiej w temat
Regularne wyrażenia pojawiły się w latach 50. i od tego czasu są kluczowym narzędziem w programowaniu. W Lua, regularne wyrażenia są częścią biblioteki standardowej. Alternatywy to np. zewnętrzne biblioteki jak LPEG. Implementacja w Lua jest prostsza i mniej wydajna niż w innych językach, ale w zupełności wystarczająca dla większości zastosowań.

## Zobacz również
- Dokumentacja Lua 5.4: https://www.lua.org/manual/5.4/
- Wprowadzenie do regularnych wyrażeń w Lua: http://lua-users.org/wiki/PatternsTutorial
- Biblioteka LPEG: http://www.inf.puc-rio.br/~roberto/lpeg/