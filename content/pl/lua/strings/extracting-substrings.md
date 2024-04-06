---
date: 2024-01-20 17:46:09.645550-07:00
description: "How to: (Jak to zrobi\u0107:) W Lua, metoda `string.sub` jest klasycznym\
  \ podej\u015Bciem do wyci\u0105gania podci\u0105g\xF3w. Historia j\u0119zyka Lua,\
  \ powsta\u0142ego w Brazylii w\u2026"
lastmod: '2024-04-05T22:50:49.851135-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) W Lua, metoda `string.sub` jest klasycznym podej\u015B\
  ciem do wyci\u0105gania podci\u0105g\xF3w."
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
weight: 6
---

## How to: (Jak to zrobić:)
```Lua
-- Prosta funkcja do wyciągania podciągu
local tekst = "Witaj w świecie Lua!"

-- Wyciągamy słowo 'świecie' (początek na 8 znaku, koniec na 14)
local podciag = tekst:sub(8, 14)

print(podciag) -- Wyświetla 'świecie'
```

```Lua
-- Użycie negatywnych indeksów do wyciągnięcia końcówki
local przywitanie = "Cześć, jak się masz?"

-- Wyciągamy 'masz?' (początek 4 znaki od końca)
local koncowka = przywitanie:sub(-5)

print(koncowka) -- Wyświetla 'masz?'
```

## Deep Dive (Dogłębna analiza)
W Lua, metoda `string.sub` jest klasycznym podejściem do wyciągania podciągów. Historia języka Lua, powstałego w Brazylii w latach 90, pokazuje, że inspiracją były mechanizmy znane z innych języków, takich jak C. Alternatywami dla `string.sub` są bardziej kompleksowe funkcje, jak `string.match` lub `string.gmatch`, które pozwalają na wycinanie fragmentów za pomocą wyrażeń regularnych.

Lua indeksuje znaki od 1, a nie od 0 jak większość języków programowania. Implementacja `string.sub` pozwala również na użycie indeksów negatywnych, co jest przydatne przy pracy z końcówką tekstu.

## See Also (Zobacz także)
- [String Manipulation](https://www.lua.org/manual/5.4/manual.html#6.4) - Oficjalne informacje o manipulacji ciągami w Lua.
- [Programming in Lua (Edition 4)](https://www.lua.org/pil/contents.html) - Książka oferująca rozległą wiedzę o programowaniu w Lua, w tym o ciągach znaków.
