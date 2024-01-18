---
title:                "Interpolacja ciągu znaków"
html_title:           "Lua: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Interpolacja ciągu znaków to proces łączenia zmiennych z łańcuchami znaków w celu stworzenia nowego łańcucha znaków. Programiści często używają interpolacji ciągu znaków, ponieważ ułatwia to tworzenie dynamicznych i zmieniających się łańcuchów znaków w swoim kodzie.

## Jak to zrobić:

### Przykład 1:
```Lua
-- deklarowanie dwoch zmiennych
local imie = "Anna"
local nr = 1

-- interpolacja ciągu znaków 
local wynik = "Witaj, jestem " .. imie .. "! Jestem osobą numer " .. nr

-- wydrukowanie wyniku
print(wynik)
```

#### Wynik:
```
Witaj, jestem Anna! Jestem osobą numer 1
```

### Przykład 2:
```Lua
-- deklarowanie zmiennej
local liczba = 3.14

-- interpolacja ciągu znaków
local wynik = "Liczba PI jest równa " .. liczba

-- wydrukowanie wyniku z dokładnością do 2 miejsc po przecinku
print(string.format("%.2f", wynik))
```

#### Wynik:
```
Liczba PI jest równa 3.14
```

## Głębsze zagadnienia:

### Kontekst historyczny:
Interpolacja ciągu znaków jest popularną techniką używaną przez programistów od wielu lat. Początkowo wykorzystywana w językach programowania nie obsługujących konkatenacji, interpolacja stała się powszechnie używanym sposobem tworzenia dynamicznych łańcuchów znaków w wielu językach.

### Alternatywy:
Alternatywą dla interpolacji ciągu znaków jest konkatenacja. W obu przypadkach można osiągnąć podobne wyniki, jednak interpolacja jest bardziej czytelna i skuteczna.

### Szczegóły implementacji:
W języku Lua interpolacja ciągu znaków jest możliwa za pomocą operatora konkatenacji ```..```, który łączy dwa łańcuchy znaków w jeden. Można także używać funkcji ```string.format()``` do formatowania wyjściowego łańcucha znaków.

## Zobacz też:

Oficjalna dokumentacja języka Lua: https://www.lua.org/docs.html

Przykładowe rozwiązania problemów z użyciem interpolacji ciągu znaków w Lua: https://rosettacode.org/wiki/Interpolated_string_interpolation#Lua