---
title:                "Tworzenie pliku tekstowego"
html_title:           "Lua: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego to robić?

Pisanie pliku tekstowego jest jedną z podstawowych czynności programistycznych. Polega ono na zapisaniu informacji w postaci tekstu do pliku, który może być odczytany przez inne programy. Programiści często korzystają z tej metody, aby magazynować dane, przetwarzać je lub udostępniać innym użytkownikom.

## Jak to zrobić?

Pisanie pliku tekstowego w Lua jest bardzo proste i wymaga użycia kilku prostych poleceń. Najpierw musimy otworzyć plik, do którego będziemy zapisywać tekst. W tym celu wykorzystujemy funkcję `io.open()` i podajemy jako argument nazwę pliku oraz tryb, w jakim ma zostać otwarty, np. "w" oznacza tryb zapisu. Następnie, wykorzystując funkcję `file:write()`, możemy wpisać dowolny tekst do pliku, a na koniec musimy go zamknąć za pomocą funkcji `file:close()`. Poniżej znajduje się przykładowy kod:

```Lua
local file = io.open("plik.txt", "w")
file:write("Przykładowy tekst do zapisania w pliku.")
file:close()
```

Po uruchomieniu tego kodu, w folderze, w którym znajduje się nasz program, powinien pojawić się plik o nazwie "plik.txt" zawierający wpisany przez nas tekst.

## Deep Dive

Pisanie pliku tekstowego jest jedną z najprostszych metod zapisu danych w Lua. Alternatywne metody to m.in. korzystanie z biblioteki `io` lub funkcji `print()` i przekierowywanie wyjścia do pliku za pomocą operatora ">".

Podczas pisania pliku tekstowego, warto pamiętać o trybie, w jakim go otwieramy. Otwarcie pliku w trybie "w" spowoduje nadpisanie istniejącego pliku lub, jeśli plik nie istnieje, utworzenie nowego. Jeśli natomiast chcemy dopisać tekst do istniejącego pliku, możemy otworzyć go w trybie "a".

## Zobacz też

- [Oficjalna dokumentacja Lua](https://www.lua.org/docs.html)
- [Pisanie plików tekstowych w języku Lua](https://www.tutorialspoint.com/lua/lua_file_io.htm)
- [Książka "Programowanie w języku Lua"](http://www.wydawnictwohelion.pl/book/174/programowanie-w-jezyku-lua-praktyka-ewolucyjna.html)