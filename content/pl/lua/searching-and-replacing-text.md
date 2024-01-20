---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

```markdown
## Co i Dlaczego?
Poszukiwanie i zamiana tekstu to operacje używane do znalezienia i zmiany specyficznych ciągów znaków w pliku czy programie. Programiści robią to, aby szybko aktualizować, korygować lub ulepszać kod.

## Jak to zrobić?
W Lua możemy użyć funkcji `string.gsub` do wyszukiwania i zastępowania tekstu. Oto przykład:

```Lua
-- Przykład funkcji string.gsub
local str = "Witaj, świecie!"
local find = "świecie"
local replace = "Lua"

local result = string.gsub(str, find, replace)
print(result)
```
Wyjście programu to:
```
Witaj, Lua!
```
W powyższym kodzie, zmienna `find` to ciąg, którego szukamy, a zmienna `replace` to ciąg, którym chcemy go zastąpić. Funkcja `string.gsub` zwraca nowy ciąg z wykonanymi zmianami.

## Głębsze spojrzenie
Poszukiwanie i zamiana tekstu mają swoje korzenie w tradycyjnych edytorach tekstu Unix, takich jak ed i vi. W Lua funkcja `gsub` pochodzi od wyrażeń regularnych używanych w tych edytorach. Alternatywą może być użycie funkcji `string.find` i `string.sub` do wykonania tych samych operacji, choć jest to bardziej złożone. Implementacja `string.gsub` w Lua jest efektywna, gdyż używa algorytmu Knutha-Morrisa-Pratta do wyszukiwania podciągów.

## Zobacz także
Dokumentację języka Lua na temat ciągów znaków znajdziesz [tutaj](https://www.lua.org/manual/5.1/manual.html#5.4).
Oto [link](https://www.lua.org/pil/20.2.html) do więcej informacji o ciągach w Lua oraz metodach takich jak `gsub`, `find` i `sub`.
```
Remember to always use the Polish versions of section headings ("## Co i Dlaczego?", "## Jak to zrobić?", "## Głębsze spojrzenie", "## Zobacz także") in Polish articles.