---
title:                "Zamiana liter na wielkie w łańcuchu znaków"
aliases:
- /pl/lua/capitalizing-a-string/
date:                  2024-02-03T19:05:57.350233-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zamiana liter na wielkie w łańcuchu znaków"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Wielką literą zaczyna się każde słowo w zdaniu, modyfikując pierwszy znak każdego słowa na wielką literę, jednocześnie zapewniając, że pozostałe są małe. Ta technika jest powszechnie stosowana do formatowania tekstu na potrzeby bardziej profesjonalnej lub czytelnej prezentacji, takich jak przygotowywanie tytułów lub danych wejściowych użytkownika do wyświetlenia.

## Jak to zrobić:
Lua nie posiada wbudowanej funkcji do zmiany wielkości liter w ciągu, ale możesz łatwo osiągnąć ten cel, używając podstawowych funkcji manipulacji ciągami. Oto prosta funkcja do zamiany pierwszej litery pojedynczego słowa na wielką:

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hello"))  -- Wyjście: Hello
```

Aby zamienić pierwszą literę każdego słowa w zdaniu na wielką, możesz podzielić zdanie na słowa, zamienić każde z nich, a następnie połączyć je ponownie:

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hello world from lua"))  -- Wyjście: Hello World From Lua
```

Jeśli pracujesz nad projektem, w którym kluczowa jest wydajność i znajdziesz się w potrzebie zaawansowanych możliwości manipulacji ciągami, rozważ użycie biblioteki innej firmy, jak `Penlight`. Penlight wzbogaca Lua o bardziej wszechstronne funkcje obsługi ciągów, wśród innych narzędzi:

```lua
-- Zakładając, że Penlight jest zainstalowany:
local pl = require("pl.stringx")
local text = "hello lua users"
text = pl.capitalized(text)
print(text)  -- Wyjście: Hello lua users

-- Uwaga: Funkcja capitalized z Penlight zmienia na wielką literę tylko pierwsze słowo.
-- Do zmiany wielkości liter każdego słowa, nadal byłoby potrzebne zaimplementowanie własnego rozwiązania lub eksploracja innych bibliotek.
```
