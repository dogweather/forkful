---
title:                "Korzystanie z wyrażeń regularnych"
html_title:           "Lua: Korzystanie z wyrażeń regularnych"
simple_title:         "Korzystanie z wyrażeń regularnych"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co to jest & dlaczego warto?

Regular expressions (wyrażenia regularne) są narzędziem programistycznym, które pozwala na wyszukiwanie i manipulowanie tekstu na podstawie określonych wzorców. Programiści najczęściej korzystają z wyrażeń regularnych w celu analizy i weryfikacji danych w aplikacjach, takich jak formularze internetowe czy systemy przetwarzania tekstu.

## Jak to zrobić?

```Lua
-- Przykładowe wyrażenie regularne, które znajduje znaki alfanumeryczne w tekście:
local pattern = "[a-zA-Z0-9]+"
-- Przykładowy tekst, na którym będzie stosowane wyrażenie regularne:
local text = "Hello123 World!"
-- Wykorzystanie funkcji string.match() do znalezienia dopasowania:
local match = string.match(text, pattern)
--Wynik:
"Hello123"
```

```Lua
-- Przykładowe wyrażenie regularne, które znajduje adresy URL w tekście:
local pattern = "https?://[%w-_%.%?%.:@!%&/%+%=~#]+"
-- Przykładowy tekst, na którym będzie stosowane wyrażenie regularne:
local text = "Check out this website: https://www.example.com"
-- Wykorzystanie pętli for do znalezienia wszystkich dopasowań:
for match in string.gmatch(text, pattern) do
  --Wypisanie znalezionych dopasowań:
  print(match)
end
-- Wynik:
"https://www.example.com"
```

##Głębsze wgląd

Regular expressions zostały wynalezione w latach 50. XX wieku przez matematyka Stephena Kleene i od tego czasu są szeroko wykorzystywane w różnych językach programowania. Alternatywami dla wyrażeń regularnych w Lua są m.in. wzorce string.match() i string.gmatch(), jednak wyrażenia regularne pozwalają na bardziej zaawansowane manipulowanie tekstem.

Implementacja wyrażeń regularnych w Lua opiera się na wykorzystaniu biblioteki LPeg, dzięki czemu wyrażenia te są łatwe w użyciu i bardzo wydajne.

## Zobacz także

Więcej informacji na temat wyrażeń regularnych w Lua można znaleźć na oficjalnej dokumentacji Lua oraz na stronie projektu LPeg:  
https://www.lua.org/docs.html  
https://www.inf.puc-rio.br/~roberto/lpeg/