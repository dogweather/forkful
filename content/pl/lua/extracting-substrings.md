---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Ekstrakcja podciągów to proces wyodrębniania małej części z większego łańcucha znaków. Programiści robią to, żeby manipulować danymi i używać ich w inne, wygodniejsze sposoby.

## Jak to zrobić?

Łańcuchów w Lua można używać z funkcją `string.sub`. Oto jak to działa:

```Lua
tekst = "Cześć, jestem programistą Lua"
podciag = string.sub(tekst, 8, 12)
print(podciag)
```

Wyjście:

```
jestem
```

Funkcja `string.sub` bierze trzy argumenty - łańcuch do przetworzenia, indeks początku podciągu (licząc od 1), i indeks końca podciągu.

## Deep Dive

- Kontekst historyczny: Funkcja `string.sub` jest częścią standardowej biblioteki Lua praktycznie od początku.
- Alternatywy: Lua nie oferuje bezpośrednich alternatyw do `string.sub`, ale możesz skorzystać z biblioteki `string.gsub` do wykonania podobnych operacji.
- Szczegóły implementacji: Funkcja `string.sub` jest zaimplementowana w C i jest to klucz do jej wydajności.

## Zobacz także

- Oficjalna dokumentacja Lua na temat łańcuchów znaków: http://www.lua.org/manual/5.4/manual.html#6.4
- Przewodnik po Lua dla początkujących: https://www.lua.org/pil/20.html
- Więcej przykładów użycia `string.sub`: https://www.tutorialspoint.com/lua/lua_strings.htm