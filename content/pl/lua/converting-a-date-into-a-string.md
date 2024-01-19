---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego? 

Konwersja daty na łańcuch znaków zadaniem, które polega na przekształceniu wartości daty w czytelny ciąg tekstowy. Programiści robią to, ponieważ ułatwia to manipulację i prezentację daty w interfejsie użytkownika.

## Jak to zrobić:

```Lua
os.setlocale('pl_PL') -- ustawiamy lokalizację na Polską
d = os.date("*t") --pobieramy obecny czas i datę do zmiennej

-- tworzymy string z datą
string_data = string.format("Dzisiaj jest %d-%d-%d, godzina %d:%d:%d", d.year, d.month, d.day, d.hour, d.min, d.sec)
print(string_data)  -- wyświetlamy datę
```
Przykładowe wyjście:
```Lua
Dzisiaj jest 2021-7-8, godzina 18:30:15
```

## Pogłębione Informacje:

1) Kontekst historyczny: Pierwotnie w Lua nie było natywnej obsługi dat i czasu. Funkcja `os.date` została wprowadzona dopiero w Lua 5.1.

2) Alternatywy: Możemy skorzystać z bibliotek zewnętrznych, takich jak date.lua dla bardziej skomplikowanych manipulacji datą.

3) Szczegóły implementacji: Funkcja `os.date` konwertuje liczbę sekund od 1 stycznia 1970 roku (czasu Unixowego) na strukturę `tm` C. Z tego tworzymy string.

## Zobacz też:

1) Dokumentacja Lua: [os.date](https://www.lua.org/manual/5.1/manual.html#pdf-os.date), [string.format](https://www.lua.org/pil/20.2.html)

2) Biblioteka daty dla Lua: [date.lua](https://github.com/Tieske/date)

3) Praca z datą i czasem w Lua: [Tutoriale](https://www.tutorialspoint.com/lua/lua_dates_time.htm)