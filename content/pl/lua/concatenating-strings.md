---
title:                "Konkatenacja ciągów znaków"
html_title:           "Bash: Konkatenacja ciągów znaków"
simple_title:         "Konkatenacja ciągów znaków"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?
Konkatenacja łańcuchów to proces łączenia dwóch lub więcej łańcuchów w jednym. Programiści używają go do łączenia odpowiedzi, wyświetlania informacji na ekranie, generowania kodu i wiele innych.

## Jak to zrobić:
Możemy skorzystać z operatora '..' w Lua do połączenia dwóch stringów.

```Lua
str1 = "Cześć, "
str2 = "świecie!"

-- Łączenie string’ów
str3 = str1 .. str2
print(str3)
```

Twój output będzie: `Cześć, świece!`

## Wgłębna analiza
Konkatenacja łańcuchów jest w Lua od samego początku, efektem dziedziczenia po językach, takich jak Python czy JavaScript. Oprócz operatora `..`, mamy inną metodę: `string.format()`.

```Lua
str1 = "Cześć"
str2 = "świecie"
str3 = string.format("%s, %s!", str1, str2)

print(str3)
```

Output będzie taki sam: `Cześć, świece!`

Jednak metoda ta jest bardziej skomplikowana i wymaga więcej pamięci operacyjnej, co może być problemem dla bardziej rozbudowanych programów.

## Zobacz też
* [Programowanie Lua 5.1](https://www.lua.org/pil/11.6.html) - Przeczytaj więcej o formatach string’ów w dokumentacji Lua.
* [Dyskusja na Stackoverflow o konkatenacji](https://stackoverflow.com/questions/3305771/lua-string-concatenation-and-memory) - Więcej informacji o konkatenacji i jej wpływ na pamięć w Lua.