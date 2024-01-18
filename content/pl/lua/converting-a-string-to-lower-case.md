---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "Lua: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Konwersja tekstu na małe litery jest procesem zmiany wszystkich liter w danym tekście na ich mniejsze odpowiedniki. Programiści często wykonują tę operację, aby ujednolicić dane wejściowe lub ułatwić wyszukiwanie i porównywanie tekstu.

## Jak to zrobić:

Dostępne są dwa sposoby na przekonwertowanie tekstu na małe litery w języku Lua. Pierwszym sposobem jest użycie funkcji `string.lower()` która zwraca przekonwertowany łańcuch. Przykładowy kod wygląda następująco:

```Lua
local tekst = "PRZYKŁADOWY TEKST"
print(string.lower(tekst))
```

Wynik wydruku będzie następujący:

```Lua
przykładowy tekst
```

Drugim sposobem jest wykorzystanie metody `lower()` na zmiennej typu string. Przykładowy kod wygląda następująco:

```Lua
local tekst = "PRZYKŁADOWY TEKST"
print(tekst:lower())
```

Wynik wydruku będzie taki sam jak w przypadku pierwszego sposobu.

## Głęboka Analiza:

Konwersja tekstu na małe litery jest często stosowana w celu porównywania tekstu bez uwzględnienia wielkości liter. Jest to szczególnie przydatne w wyszukiwaniu i sortowaniu danych. W języku Lua dostępna jest również funkcja `string.upper()` która przekonwertuje tekst na wielkie litery. Alternatywnym sposobem na zmianę wielkości liter jest użycie modułu `string` w celu wywołania odpowiedniej metody przy użyciu `string.lower` lub `string.upper`.

## Zobacz także:

[Oficjalna dokumentacja Lua](https://www.lua.org/manual/5.3/manual.html#6.4.2)

[Porównywanie i sortowanie danych w Lua](https://www.lua.org/pil/19.3.html)