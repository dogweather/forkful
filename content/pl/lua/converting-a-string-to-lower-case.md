---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Konwersja łańcucha znaków na małe litery to operacja programistyczna, która przekształca wszystkie duże litery łańcucha w małe litery. Programiści robią to, aby ułatwić porównywanie łańcuchów, ignorując wielkość liter.

## Jak to zrobić:
Prosty przykład pokazujący, jak w Lua przekształcić łańcuch na małe litery:

```Lua
lowercaseString = string.lower("TWOJEJ TEKST DO KONWERSJI")
print(lowercaseString)
```
Wyjście z powyższego kodu:

```Lua
"twojej tekst do konwersji"
```
## Głębsza Analiza
Różne języki programowania mają różne sposoby konwersji łańcuchów na małe litery. Historia funkcji `string.lower` w Lua sięga wczesnych dni tego języka. W praktyce `string.lower` korzysta z mapowania ASCII. Alternatywą może być napisanie własnej funkcji, która przemierza łańcuch i konwertuje każdy znak indywidualnie. Jednak, dla większości przypadków, `string.lower` jest najprostszym i najbardziej efektywnym rozwiązaniem. 

## Zobacz tez
1. Dokumentacja Lua: [String Manipulation](https://www.lua.org/pil/20.html)
2. Stack Overflow: [Lowercase a String in Lua](https://stackoverflow.com/questions/20284515/lowercase-a-string-in-lua)