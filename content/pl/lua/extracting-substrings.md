---
title:                "Wyciąganie podciągów"
html_title:           "Lua: Wyciąganie podciągów"
simple_title:         "Wyciąganie podciągów"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Wyciąganie podciągów to proces wyodrębniania fragmentów tekstu z ciągu znaków. Programiści często wykonują tę czynność, aby uzyskać pożądaną informację lub zmienić układ tekstu.

## How to:

```lua 
-- Przykładowy ciąg znaków
local tekst = "Witaj w świecie programowania"

-- Wyciąganie podciągu od znaku 7 do końca
local podciag = string.sub(tekst, 7)

-- Wyciąganie podciągu od znaku 7 do 13
local podciag2 = string.sub(tekst, 7, 13)

print(podciag) -- "świecie programowania"
print(podciag2) -- "świecie"
```

## Deep Dive:
Wyciąganie podciągów jest powszechnie stosowane w programowaniu, ponieważ daje możliwość manipulacji tekstem na wiele różnych sposobów. Wcześniej istniało wiele różnych funkcji do tego celu, takich jak ```substr``` czy ```substring```, ale w nowych wersjach języka Lua została wprowadzona funkcja ```string.sub```, która jest zalecanym sposobem na wyciąganie podciągów.

Inną alternatywą jest wykorzystanie wyrażeń regularnych, ale zazwyczaj jest to mniej wydajne i trudniejsze do zrozumienia. W języku Lua istnieje również funkcja ```string.match```, która może być użyta do wyodrębnienia podciągów na podstawie wzorców.

Funkcja ```string.sub``` przyjmuje jako argumenty ciąg znaków, pozycję początkową oraz ewentualnie pozycję końcową. Jeśli nie podamy pozycji końcowej, zostaną wybrane wszystkie znaki od podanej pozycji do końca tekstu.

## See Also:
- [Dokumentacja języka Lua](https://www.lua.org/manual/5.4/manual.html#6.4)
- [Przykłady wyciągania podciągów w języku Lua](https://www.lua.org/pil/20.2.html)