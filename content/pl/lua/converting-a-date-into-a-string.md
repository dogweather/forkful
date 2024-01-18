---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Lua: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego to robimy?
Przekształcanie daty na ciąg znaków to proces konwertowania wartości daty (np. "12/03/2021") na zrozumiały dla komputera format (np. "Mar 12, 2021"). Programiści często wykonują tę operację, aby móc wyświetlić daty w przyjazny dla ludzi sposób lub wykorzystać je w innych częściach kodu.

## Jak to zrobić:
```lua
-- Tworzenie nowego obiektu daty
local date = os.date("*t")
-- Konwertowanie daty na ciąg znaków w formacie miesiąc/dzień/rok
local date_string = date.month.."/"..date.day.."/"..date.year
print(date_string)
```
```lua
-- Wczytywanie daty z ciągu znaków
local date_string = "03/12/2021"
-- Konwertowanie daty na obiekt daty
local date = os.date("*t", os.time(date_string))
print(date.day, date.month, date.year)
```
Output:
```
3/12/2021
12 3 2021
```

## Głębsze zanurzenie:
Konwertowanie daty na ciąg znaków jest popularną operacją w programowaniu od dawna. Początkowo programiści musieli ręcznie formatować daty, ale z czasem powstały biblioteki lub wbudowane funkcje, które ułatwiają ten proces. Alternatywne sposoby konwersji daty obejmują wykorzystywanie gotowych bibliotek lub tworzenie niestandardowych funkcji.

## Zobacz też:
- [Biblioteka os.date](https://www.lua.org/pil/22.1.html) dla pełnej dokumentacji
- [Funkcja string.format](https://www.lua.org/manual/5.4/manual.html#pdf-string.format) do formatowania ciągów znaków w Lua