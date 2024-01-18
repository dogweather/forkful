---
title:                "Utilizando expressões regulares"
html_title:           "Lua: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que e por que?
Expressões regulares são um recurso importante para programadores que desejam realizar buscas e substituições de texto de forma eficiente. Elas permitem criar padrões de texto que podem ser usados para encontrar, filtrar ou alterar determinadas partes de uma string. Os programadores usam expressões regulares para economizar tempo e evitar a criação de funções complexas para manipulação de texto.

## Como fazer:
Veja abaixo alguns exemplos de como usar expressões regulares em Lua:

```Lua
-- Encontrar todas as instâncias do padrão "maçã" em uma string
local str = "Eu quero uma maçã verde e suculenta."
local matches = string.match(str, "maçã")
print(matches) --> maçã

-- Substituir todas as letras maiúsculas por minúsculas
str = "Olá, Mundo!"
local replaced = string.gsub(str, "%u", function(c)
    return string.lower(c)
end)
print(replaced) --> olá, mundo!
```

## Mergulho profundo:
As expressões regulares foram inventadas na década de 1950 por Stephen Cole Kleene, um matemático americano. Elas podem parecer confusas no início, mas com prática se tornam uma ferramenta poderosa para manipulação de texto. Além disso, existem alternativas para expressões regulares em Lua, como o uso de funções de manipulação de string, mas elas são menos eficientes e complexas. As expressões regulares em Lua seguem a sintaxe padrão do conjunto de linguagens de programação POSIX.

## Veja também:
- [Documentação oficial de strings em Lua](https://www.lua.org/manual/5.4/manual.html#6.4.2)
- [Tutorial sobre expressões regulares em Lua](https://anenadic.github.io/regular-expressions/regular-expressions-in-lua.html)
- [Ferramenta online para testar expressões regulares em Lua](https://regex101.com/r/8cFjZA/1/)