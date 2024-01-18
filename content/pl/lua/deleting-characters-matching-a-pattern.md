---
title:                "Usuwanie znaków pasujących do wzoru"
html_title:           "Lua: Usuwanie znaków pasujących do wzoru"
simple_title:         "Usuwanie znaków pasujących do wzoru"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Usuwanie znaków odpowiadających wzorcowi to proces usuwania określonych znaków z ciągów znaków wg ustalonego wzorca. Programiści stosują tę technikę w celu uproszczenia i oczyszczenia danych oraz przyspieszenia działania programów.

## Jak to zrobić:
Przykładowy kod i wynik działania wewnątrz bloków kodu ```Lua ... ```

Usuwanie wszystkich znaków "a" z ciągu znaków:
```Lua
string.gsub("Kana", "a", "")
-- Output: Kn
```

Usuwanie cyfr z ciągu znaków:
```Lua
string.gsub("65abc29", "%d", "")
-- Output: abc
```

Usuwanie polskich znaków diakrytycznych z ciągu znaków:
```Lua
string.gsub("żółw", "[^a-zA-Z ]", "")
-- Output: żółw
```

## Głębsza Analiza:
W przeszłości popularne było używanie funkcji `strrep`, która powtarzała dany znak (lub ciąg) określoną ilość razy. Jednak metoda ta jest mniej efektywna niż `gsub`, która pozwala usunąć wybrane znaki, a nie tylko powtórzyć je wielokrotnie.

Alternatywą dla `gsub` może być użycie wyrażeń regularnych w języku Lua. Pozwalają one na jeszcze większe dopasowanie wzorca do znaków do usunięcia.

Warto pamiętać, że `gsub` operuje na ciągach znaków, a nie na pojedynczych znakach. Jeśli chcesz usunąć pojedynczy znak, może to być zrobione poprzez prostszą funkcję `string.sub`.

## Zobacz również:
- [Dokumentacja Lua](https://www.lua.org/docs.html)
- [Tutorial Lua](https://www.tutorialspoint.com/lua/)
- [Wyrażenia regularne w języku Lua](https://www.tutorialspoint.com/lua/lua_regular_expressions.htm)