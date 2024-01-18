---
title:                "Unindo strings"
html_title:           "Lua: Unindo strings"
simple_title:         "Unindo strings"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que & Por quê?

A concatenação de strings é quando você combina duas ou mais strings em uma única string. Os programadores geralmente fazem isso para criar mensagens personalizadas ou para unir informações em um formato específico.

## Como fazer:

```Lua
-- Exemplo 1: Usando o operador de concatenação ".." 
local nome = "João"
local sobrenome = "Silva"
print("Olá " .. nome .. " " .. sobrenome) -- saída: Olá João Silva

-- Exemplo 2: Usando a função string.format()
local preço = 25
local produto = "livros"
print(string.format("O preço dos %s é %d reais.", produto, preço)) -- saída: O preço dos livros é 25 reais

-- Exemplo 3: Usando a função table.concat()
local frutas = {"maçã", "banana", "morango"}
print("Eu gosto de " .. table.concat(frutas, ", ")) -- saída: Eu gosto de maçã, banana, morango
```

## Mergulho Profundo:

A concatenação de strings existe há muito tempo e é uma técnica amplamente utilizada em muitas linguagens de programação. Além dos métodos mencionados acima, também é possível realizar a concatenação usando a função string.concat() ou através do uso de placeholders em strings.

Embora a concatenação de strings seja uma técnica útil, é importante lembrar que ela pode tornar seu código menos eficiente, pois a criação de novas strings a partir de strings existentes pode consumir muita memória e tempo de processamento. Em algumas situações, é mais eficiente usar a função string.format() ou a função table.concat() para combinar várias strings.

## Veja também:

- [Documentação oficial do Lua sobre concatenação de strings](https://www.lua.org/manual/5.4/manual.html#6.4)
- [Tutorial sobre manipulação de strings em Lua](https://www.tutorialspoint.com/lua/lua_strings.htm)
- [Exemplos práticos de concatenação de strings em Lua](https://www.sitepoint.com/lua-string-manipulation-tutorial/)