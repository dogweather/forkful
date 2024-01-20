---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Parsowanie daty z ciągu to proces, w którym ciąg tekstowy jest analizowany i przekształcany na strukturę daty. Programiści robią to, aby móc manipulować i operować na datach w bardziej logiczny i efektywny sposób.

## Jak to zrobić:

Możemy użyć wbudowanej funkcji `os.date`. Ta funkcja przyjmuje format daty i ciąg do przetworzenia. Przykład poniżej pokazuje jak to zrobić:

```Lua
dateString = "07/27/2021"
format="%m/%d/%Y"
result = os.date("*t", os.time{year=string.sub(dateString, 7, 10), month=string.sub(dateString, 1, 2), day=string.sub(dateString, 4, 5)})
print(result.day.."/"..result.month.."/"..result.year)
```
Wyjście:

```Lua
27/7/2021
```
## Głębsze zanurzenie:

1. Kontekst historyczny: Lua, stworzona w 1993 r., ma wiele wbudowanych funkcji do manipulacji na datach. Podczas gdy wiele języków ma biblioteki do obsługi tego zadania, Lua ma wbudowane funkcjonalności.
2. Alternatywy: Można użyć innych funkcji, takich jak `os.time` lub `os.difftime`, aby przekształcić ciąg na datę lub porównać dwie daty.
3. Detale implementacji: `os.date` zwraca ciąg lub tabelę, w zależności od użytego formatu. `"%t"` zwraca tabelę, `"%s"` zwraca ciąg.

## Zobacz także:

1. [Dokumentacja Lua: os.date](https://www.lua.org/manual/5.3/manual.html#pdf-os.date)
2. [Poradnik Obsługi Dat i Czasu w Lua](https://riptutorial.com/lua/topic/4040/date-and-time)

Uwaga: W tym artykule podane przykłady kodu są dla Lua 5.3.