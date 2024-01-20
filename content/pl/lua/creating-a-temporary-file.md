---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Tworzenie tymczasowych plików polega na generowaniu plików o krótkim cyklu życia, które są wykorzystywane na czas odpalenia skryptu. Programiści robią to w celu przechowywania tymczasowych danych, które nie są potrzebne między sesjami, a także do testowania.
  
## Jak to zrobić:

Tworzenie tymczasowego pliku w języku Lua jest proste. Używamy do tego funkcji `os.tmpname`, której zadaniem jest zwrócenie nazwy dla nowego pliku tymczasowego.

```Lua
local tmp_file = os.tmpname()
print(tmp_file)
```
Wywołanie powyższego kodu wygeneruje wynik podobny do tego:

```Lua
/tmp/lua_3n2cb2
```
Zauważ, że ta ścieżka pliku jest tylko przykładowa i prawdopodobnie będzie inna na twoim systemie.

##O pogłębionej wiedzy 

Funkcja `os.tmpname` to część standardowych bibliotek lua od wersji 5.1, co pokazuje jej długą historię ułatwiania tworzenia plików tymczasowych developerom. Alternatywą dla `os.tmpname` może być generowanie plików z unikalnymi nazwami za pomocą własnego skryptu, choć ta metoda jest bardziej narażona na błędy. Funkcja `os.tmpname`, korzysta bezpośrednio z funkcjonalności systemu operacyjnego do tworzenia plików tymczasowych, co gwarantuje bardziej stabilne wyniki.

##Zobacz też
Dokumentacja Lua:
https://www.lua.org/manual/5.4/manual.html#6.9
Przykłady korzystania z plików tymczasowych:
http://lua-users.org/wiki/TempLibrary