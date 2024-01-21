---
title:                "Odczytywanie argumentów linii poleceń"
date:                  2024-01-20T17:56:23.750393-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie argumentów linii poleceń"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Czytanie argumentów z linii poleceń pozwala naszym skryptom Lua na interakcję z użytkownikiem lub innymi programami poprzez parametry uruchomieniowe. Programiści wykorzystują tę technikę, by skrypty były elastyczne i mogły dostosować swoje działanie w zależności od potrzeb.

## How to: (Jak to zrobić:)
Oto jak łatwo przeczytać argumenty z linii poleceń w Lua:

```Lua
-- save as script.lua
for index, value in ipairs(arg) do
    print("Argument", index, ":", value)
end
```

Uruchom skrypt, przekazując argumenty:
```
$ lua script.lua hello world
```

Wynik:
```
Argument 1 : hello
Argument 2 : world
```

## Deep Dive (Dogłębna analiza)
W przeszłości Lua używała globalnej tablicy `arg` do przechowywania argumentów linii poleceń, i nadal tak jest. W Lua 5.0 `arg` stał się oficjalny. Tablica zawiera indeksy od -n do n, gdzie 0 to ścieżka do skryptu, a pozostałe indekty odnoszą się do kolejnych argumentów. Alternatywą może być użycie `...` w skryptach i wtedy informacje o argumentach przekazywane są jako pojedyncze wartości, co jest użyteczne w funkcjach. Co do implementacji, ważne jest, aby pamiętać, że pierwszy argument (pod indeksem 1) to pierwszy argument po nazwie skryptu, a nie sama nazwa skryptu (ta jest pod indeksem 0).

## See Also (Zobacz także)
- [Oficjalna dokumentacja Lua](http://www.lua.org/manual/5.4/manual.html#6.1)
- [Stack Overflow: Przekazywanie argumentów do skryptu Lua](https://stackoverflow.com/questions/4537269/how-to-interpret-parameters-passed-to-lua-script)