---
title:                "Odczytywanie argumentów z wiersza poleceń"
html_title:           "Lua: Odczytywanie argumentów z wiersza poleceń"
simple_title:         "Odczytywanie argumentów z wiersza poleceń"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Co to jest wczytywanie argumentów wiersza poleceń i dlaczego programiści to robią?

Wczytywanie argumentów wiersza poleceń jest sposobem na uzyskanie danych od użytkownika podczas uruchamiania programu z wiersza poleceń. Jest to przydatne, gdy chcemy, aby program był bardziej interaktywny i dostosowalny do różnych przypadków użycia.

Jak to zrobić:

```lua
-- Przykładowy program, który wczytuje argumenty wiersza poleceń i wyświetla je jako tabelę
local args = {...} -- zmienna args przechowuje wszystkie argumenty wiersza poleceń (pierwszy argument będzie miał indeks 1, drugi 2, itd.)

print("Przekazane argumenty:")
for i, v in ipairs(args) do -- używamy pętli for do przeiterowania przez wszystkie argumenty
    print("Argument " .. i .. ": " .. v) -- wyświetlamy numer argumentu i jego wartość
end
```

Przykładowe wyjście:

```
Przekazane argumenty:
Argument 1: pierwszy
Argument 2: drugi
Argument 3: trzeci
```

Głębsza analiza:

Wczytywanie argumentów wiersza poleceń jest stosowane od dawna, jeszcze przed pojawieniem się języka Lua. Jest to popularna metoda pozwalająca na łatwe i szybkie dostarczenie danych do programu, zwłaszcza w przypadku zastosowań konsolowych.

Alternatywne sposoby wczytywania danych od użytkownika są stosowane w innych językach programowania, na przykład w języku Python istnieje wbudowany moduł "sys", który pozwala na dostęp do argumentów wiersza poleceń.

Implementacyjne detale wczytywania argumentów wiersza poleceń mogą się różnić w zależności od środowiska uruchomieniowego. Na przykład w przypadku LuaJIT, argumenty wiersza poleceń są przechowywane w osobnej globalnej tabeli "arg". Z kolei w systemie operacyjnym Windows, argumenty są odseparowane za pomocą znaku "/".

Zobacz też:

- Oficjalna dokumentacja języka Lua dotycząca wczytywania argumentów wiersza poleceń: https://www.lua.org/pil/13.4.1.html
- Przykłady użycia wczytywania argumentów wiersza poleceń w praktyce: https://www.lua.org/pil/17.1.html