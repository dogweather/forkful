---
date: 2024-01-20 17:44:43.679360-07:00
description: "Pobieranie strony internetowej to proces uruchomienia kodu, kt\xF3ry\
  \ \u015Bci\u0105ga zawarto\u015B\u0107 strony www do analizy lub przetwarzania.\
  \ Programi\u015Bci robi\u0105 to, aby\u2026"
lastmod: '2024-03-13T22:44:35.538798-06:00'
model: gpt-4-1106-preview
summary: "Pobieranie strony internetowej to proces uruchomienia kodu, kt\xF3ry \u015B\
  ci\u0105ga zawarto\u015B\u0107 strony www do analizy lub przetwarzania. Programi\u015B\
  ci robi\u0105 to, aby\u2026"
title: Pobieranie strony internetowej
weight: 42
---

## Co i dlaczego?
Pobieranie strony internetowej to proces uruchomienia kodu, który ściąga zawartość strony www do analizy lub przetwarzania. Programiści robią to, aby automatycznie zbierać dane, monitorować zmiany czy testować aplikacje.

## Jak to zrobić:
Użyjemy biblioteki `socket.http` z Lua. Sprawdzimy, czy strona jest dostępna i pobierzemy zawartość.

```Lua
local http = require("socket.http")
local body, code = http.request("http://www.example.com")

if code == 200 then
    print("Strona pobrana pomyślnie. Oto zawartość:")
    print(body)
else
    print("Nie udało się pobrać strony, kod błędu: " .. tostring(code))
end
```

Przykładowe wyjście, jeśli strona istnieje:
```
Strona pobrana pomyślnie. Oto zawartość:
<!doctype html>...
```

Jeżeli wystąpi błąd, może zobaczysz:
```
Nie udało się pobrać strony, kod błędu: 404
```

## Dogłębniejsze spojrzenie:
Pobieranie stron www ma długą historię, zaczynając od prostych skryptów aż do pełnoprawnych narzędzi jak `wget` czy `curl`. Lua może nie być pierwszym wyborem dla tego zadania, ale za pomocą `socket.http` czy innych, bardziej wyspecjalizowanych bibliotek jak `luasocket`, można to zrobić prosto i skutecznie. Wersja Lua wpływa na dostępne biblioteki - najnowsza to Lua 5.4, więc sprawdź kompatybilność. Co więcej, można korzystać z funkcji asynchronicznych, jeśli jest taka potrzeba, używając bibliotek takich jak `copas` dla bardziej zaawansowanych aplikacji.

## Zobacz także:
- Dokumentacja `socket.http`: http://w3.impa.br/~diego/software/luasocket/http.html
- Tutorial LuaSocket: http://lua-users.org/wiki/LuaSocketTutorial
- Informacje o Lua 5.4: https://www.lua.org/manual/5.4/
