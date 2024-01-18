---
title:                "Drukowanie wyjścia debugowania"
html_title:           "Lua: Drukowanie wyjścia debugowania"
simple_title:         "Drukowanie wyjścia debugowania"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Printowanie wyników debugowania jest procesem, który polega na wyświetlaniu informacji o aktualnym stanie programu podczas jego wykonywania. Programiści używają tego narzędzia, aby sprawdzić, czy ich kod działa tak, jak się spodziewają, oraz aby znaleźć błędy lub inne problemy w swoim programie.

## Jak to zrobić:
```Lua
--Przykład 1: Wyświetlenie tekstu na ekranie
print("Cześć!")
--Output: Cześć!

--Przykład 2: Wyświetlenie wartości zmiennej
local liczba = 5
print(liczba)
--Output: 5

--Przykład 3: Wyświetlenie wielu wartości
local imie = "Kasia"
local wiek = 25
local obecny_rok = 2021
print("Witaj, nazywam się " .. imie .. ", mam " .. wiek .. " lat i jest rok " .. obecny_rok)
--Output: Witaj, nazywam się Kasia, mam 25 lat i jest rok 2021

--Przykład 4: Wyświetlenie informacji o błędzie
local tablica = {1, 2, 3}
print(tablica[4])
--Output: nil - indeks poza zakresem tablicy
```

## Głębszy zanurzenie:
Printowanie debug output było używane już od początków programowania, gdyż było najprostszym sposobem sprawdzania poprawności działania programu. Jednak wraz z rozwojem technologii i narzędzi programistycznych, pojawiły się alternatywne metody debugowania, takie jak debuggery czy logi. W implementacji Lua, funkcja ```print()``` jest dostępna od wersji 5.1 i jest często używana do wyświetlania informacji na ekranie podczas debugowania.

## Zobacz również:
- [Oficjalna dokumentacja Lua](https://www.lua.org/docs.html)
- [Tutorial Lua dla początkujących](http://www.tutorialspoint.com/lua/)
- [Artykuł o debuggowaniu w Lua](https://www.ibm.com/developerworks/library/l-lua-debug/)