---
date: 2024-01-20 17:47:43.264324-07:00
description: "Jak to zrobi\u0107: W Lua d\u0142ugo\u015B\u0107 stringa dostaniesz\
  \ b\u0142yskawicznie."
lastmod: '2024-03-13T22:44:35.531017-06:00'
model: gpt-4-1106-preview
summary: "W Lua d\u0142ugo\u015B\u0107 stringa dostaniesz b\u0142yskawicznie."
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

## Jak to zrobić:
W Lua długość stringa dostaniesz błyskawicznie:

```Lua
local zdanie = "Witaj, świecie!"
local dlugosc = #zdanie
print(dlugosc)
```

Wyjście:

```
15
```

## Głębiej w temat
Długość stringa w Lua, od zawsze zwracana przez operator `#`, jest prosta i szybka. W przeciwieństwie do innych języków, nie musisz wywoływać metody czy funkcji – operator załatwi sprawę. 

Ale uwaga: w Lua indeksowanie zaczyna się od 1, a nie od 0 jak w wielu innych językach. To historyczne podejście ma korzenie w konwencjach używanych przez jego docelową grupę użytkowników – naukowców i inżynierów związanych z brazylijskim ropy naftową.

Alternatywą dla `#` może być `string.len(zdanie)`, ale to bardziej zbędny powrót do czasów, gdy Lua była młoda i jeszcze szukała najlepszych rozwiązań.

Operator `#` działa szybko bo Lua przechowuje długość stringów wewnętrznie, więc nie musi przeliczać jej za każdym razem. Ale pamiętaj: jeśli łączysz stringi lub inne zmienne dynamiczne, długość może się zmieniać.

## Zobacz również
Jeśli chcesz więcej informacji o stringach w Lua oraz o tym języku ogólnie, sprawdź:

- [Oficjalna dokumentacja Lua](https://www.lua.org/manual/5.4/)
- [Programming in Lua (książka)](https://www.lua.org/pil/contents.html)
