---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

W programowaniu nieraz musimy pozyskać aktualną datę. Czy to do logowania danych, wprowadzania odnośników czasowych czy śledzenia wydarzeń w czasie - znajomość sposobów na dostęp do bieżącej daty jest niezbędna.

## Jak to zrobić:

W Lua zdobycie bieżącej daty jest proste. Używamy do tego funkcji `os.date`.

```Lua
print(os.date())
```

Wynik powyższego kodu wyglądałby mniej więcej tak:

```Lua
Tue Sep 21 14:36:07 2021
```

Możemy też dostosować format wyjściowy, na przykład tak:

```Lua
print(os.date("%A, %B %d, %Y"))
```

Rezultatem powyższego kodu będzie data w formacie "dzień tygodnia, miesiąc dzień, rok":

```Lua
Tuesday, September 21, 2021
```

## Deep Dive

`os.date` jest częścią biblioteki OS w Lua. Pierwotnie w Lua istniała tylko funkcja `os.time`, służąca do uzyskiwania liczby sekund od pewnego punktu odniesienia (zwanego czasem epoki), zwykle od 1 stycznia 1970 roku. Funkcja `os.date` została dodana później, aby ułatwić programistom manipulowanie i wyświetlanie dat.

W przypadku większego stopnia personalizacji, możemy użyć alternatywnych bibliotek, takich jak `LuaDate` lub `chrono`. Właściwy wybór zależy od szczegółowych wymagań projektu.

## Zobacz też:

- Dokumentacja Lua: [os.date](https://www.lua.org/pil/22.1.html)
- GitHub: [LuaDate](https://github.com/Tieske/date)
- GitHub: [chrono](https://github.com/bsm/chrono.lua)