---
title:                "Extraindo subcadeias"
html_title:           "Lua: Extraindo subcadeias"
simple_title:         "Extraindo subcadeias"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Extrair substrings é uma técnica muito útil e comum em programação. Ela consiste em separar uma parte específica de uma string (conjunto de caracteres) com base em um critério pré-definido. Programadores usam essa técnica para manipular e processar strings de forma mais eficiente e precisa.

## Como fazer:

```Lua
--Exemplo 1: Extrair os caracteres da posição 3 até o final de uma string
local texto = "Lua é uma linguagem de programação"
local subtexto = texto:sub(3)
print(subtexto)
-- Output: "a é uma linguagem de programação"

--Exemplo 2: Extrair os primeiros 5 caracteres de uma string
local texto = "Exemplo de substring"
local subtexto = texto:sub(1, 5)
print(subtexto)
-- Output: "Exemp"

--Exemplo 3: Extrair uma sequência de caracteres entre dois marcadores
local texto = "Olá, @mundo!"
local marcador1 = "@"
local marcador2 = "!"
local inicio, final = texto:find(marcador1)
local subtexto = texto:sub(final+1, texto:find(marcador2)-1)
print(subtexto)
-- Output: "mundo"
```

## Mais detalhes:

Extrair substrings é uma técnica amplamente utilizada em linguagens de programação devido à sua eficiência no processamento de strings. No passado, essa tarefa era realizada manualmente usando funções como ```string.sub()```, que exigiam a especificação da posição inicial e final da substring desejada. No entanto, com as versões mais recentes do Lua, essa tarefa se tornou muito mais simples com o uso dos métodos ```string:sub()``` e ```string:find()```. Esses métodos fornecem uma sintaxe mais amigável e também são capazes de lidar com casos mais complexos de extração de substrings, como no exemplo 3.

Além disso, existem outras formas de extrair substrings em Lua, como o uso da sintaxe de fatiamento (slicing) ou expressões regulares. Cada método tem suas vantagens e desvantagens, portanto, é importante considerar qual é o mais adequado para o seu propósito específico.

## Veja também:

- [Documentação oficial do Lua sobre manipulação de strings](https://www.lua.org/pil/20.2.html)
- [Tutorial sobre extração de substrings em Lua](https://www.tutorialspoint.com/lua/lua_strings.htm)