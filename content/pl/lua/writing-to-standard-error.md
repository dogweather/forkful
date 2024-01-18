---
title:                "Pisanie do standardowego błędu"
html_title:           "Lua: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Czym to jest i dlaczego to robimy?
Pisanie do standardowego wyjścia błędów jest ważnym narzędziem w programowaniu Lua. W skrócie, pozwala nam wyświetlić informacje o błędach, ostrzeżeniach lub innych komunikatach, które mogą pomóc nam w odnalezieniu problemu w naszym kodzie. Jest to często stosowane przez programistów podczas debugowania oraz w celu ułatwienia użytkownikom zrozumienia błędów w programach.

## Jak to zrobić?
Aby wypisać informacje do standardowego wyjścia błędów w Lua, możemy użyć funkcji `io.stderr:write()`. W poniższym przykładzie używamy tej funkcji, aby wyświetlić komunikat o błędzie i jego numerze.

```Lua
io.stderr:write("Wystąpił błąd!", 123)
```

Jeśli uruchomimy ten kod, otrzymamy następujące wyjście:

```
Wystąpił błąd!123
```

Możemy również przekazać do funkcji `io.stderr:write()` więcej niż jeden argument, tak jak w poniższym przykładzie:

```Lua
io.stderr:write("Błąd numer ", 123, " - brak pliku")
```

Które spowoduje wyświetlenie następującego komunikatu:

```
Błąd numer 123 - brak pliku
```

## Glębszy zanurzenie
Pisanie do standardowego wyjścia błędów jest możliwe dzięki temu, że Lua ma wbudowane moduły `io` i `stderr`. Dzięki temu nie musimy importować żadnych dodatkowych modułów, aby móc korzystać z tej funkcji. Istnieje również alternatywny sposób na wypisywanie do standardowego wyjścia błędów - poprzez funkcję `error()`. Jednak używając `error()`, wyjście błędów zostanie przerwane, a funkcja `io.stderr:write()` pozwala na kontynuowanie działania programu pomimo wystąpienia błędu.

## Zobacz też
- [Dokumentacja Lua o standardowym wyjściu błędów](https://www.lua.org/manual/5.4/manual.html#6.8)
- [Poradnik dla początkujących programistów Lua](https://www.tutorialspoint.com/lua/lua_basic_syntax.htm)
- [Inne metody obsługi wyjścia w Lua](http://lua-users.org/wiki/StandardLibraryInOut)