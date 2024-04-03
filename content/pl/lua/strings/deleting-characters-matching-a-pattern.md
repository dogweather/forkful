---
date: 2024-01-20 17:43:02.061984-07:00
description: "How to: - Jak to zrobi\u0107: ."
lastmod: '2024-03-13T22:44:35.524048-06:00'
model: gpt-4-1106-preview
summary: .
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

## How to: - Jak to zrobić:
```Lua
local text = "Kodowanie w Lua jest łatwe i przyjemne!"
local pattern = "%a+" -- Wzorzec do znalezienia liter

-- Usuwanie znaków pasujących do wzorca
local cleaned_text = text:gsub(pattern, "")
print(cleaned_text)  -- Wypisuje: "      !"
```
Wzorce można też dostosować, na przykład:
```Lua
local text = "E-mail: przyklad@example.com, Telefon: +48 123 456 789"
local email_pattern = "[\%w%.]+@[\%w%.]+"
local phone_pattern = "%+%d%d%d%d%d%d%d%d%d%d%d"

-- Usuń adresy e-mail
local without_email = text:gsub(email_pattern, "")
print(without_email)  -- Wypisuje: ", Telefon: +48 123 456 789"

-- Usuń numery telefonów
local without_phone = text:gsub(phone_pattern, "")
print(without_phone)  -- Wypisuje: "E-mail: przyklad@example.com, Telefon: "
```

## Deep Dive - Dokładna analiza:
Lua oferuje potężne narzędzia do manipulacji tekstami przez wzorce stylizowane na wyrażenia regularne. Wersje Lua od 5.1 do najnowszej wprowadzały różne udoskonalenia, ale podstawowa funkcjonalność pozostała stabilna.

Alternatywą dla `string.gsub` jest użycie funkcji `string.match` do znalezienia pasujących fragmentów, a następnie ich usunięcie - ale to bardziej karkołomne i mniej wydajne. Warto pamiętać, że wzorce Lua nie są tak rozbudowane jak w pełni rozwinięte wyrażenia regularne, znane na przykład z Perl.

Implementacja potraktowania wzorców w Lua różni się od typowych wyrażeń regularnych – na przykład znak `%` używany jest jako escape dla specjalnych znaków, zamiast bardziej powszechnego w wyrażeniach regularnych znaku `\`.

## See Also - Zobacz również:
- [Programming in Lua (4th edition)](https://www.lua.org/pil/contents.html) - oficjalny przewodnik po języku Lua.
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/) - dokumentacja wzorców w Lua.
