---
date: 2024-01-26 03:40:37.186600-07:00
description: "Jak to zrobi\u0107: Oto jak pozby\u0107 si\u0119 cudzys\u0142ow\xF3\
  w w Lua."
lastmod: '2024-03-13T22:44:35.528007-06:00'
model: gpt-4-0125-preview
summary: "Oto jak pozby\u0107 si\u0119 cudzys\u0142ow\xF3w w Lua."
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
weight: 9
---

## Jak to zrobić:
Oto jak pozbyć się cudzysłowów w Lua:

```lua
local function remove_quotes(str)
  return (str:gsub("^%p(.*)%p$", "%1"))
end

print(remove_quotes('"Hello, World!"'))     -- Hello, World!
print(remove_quotes("'Goodbye, Quotes!'"))  -- Goodbye, Quotes!
```

I bingo! Te cudzysłowy zniknęły jak skarpetki w pralce.

## Pogłębiona analiza
Ludzie usuwali cudzysłowy z ciągów znaków odkąd języki potrafiły obsługiwać tekst, co trwa w zasadzie od zawsze. W Lua funkcja `gsub` wykonuje ciężką pracę, używając wzorców niczym skalpel do wycinania cudzysłowów. Alternatywy? Jasne, możesz użyć regex w językach, które go obsługują, albo napisać własną pętlę, która przegląda każdy znak (nuda, ale hej, to twój czas).

Dopasowywanie wzorców w Lua daje ci siłę podobną do doświadczeń z regexem-light bez potrzeby importowania całej biblioteki. Znak caret (`^`) i znak dolara (`$`) dopasowują odpowiednio początek i koniec ciągu znaków; `%p` pasuje do dowolnego znaku interpunkcyjnego. Po pozbyciu się wiodącej i końcowej interpunkcji, przechwytujemy wszystko inne za pomocą `(.*),` i zastępujemy całe dopasowanie tą grupą przechwytywania, używając `" %1"`.

Pamiętaj, że dopasowywanie wzorców w Lua nie jest tak potężne jak pełnoprawne silniki regex – na przykład, nie potrafi liczyć ani cofać się. Ta prostota jest zarówno błogosławieństwem, jak i przekleństwem, w zależności od tego, jakie cudzysłowy próbujesz uporządkować i gdzie się ukrywają.

## Zobacz także
Zanurkuj głębiej w dopasowywanie wzorców w Lua z książką PiL (Programowanie w Lua): http://www.lua.org/pil/20.2.html

Dla czystej elegancji sprawdź, jak inne języki robią to na porównanie, zaczynając od `str.strip` w Pythonie: https://docs.python.org/3/library/stdtypes.html#str.strip
