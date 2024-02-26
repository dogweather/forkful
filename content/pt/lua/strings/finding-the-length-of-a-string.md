---
date: 2024-01-20 17:47:41.283724-07:00
description: "Descobrir o comprimento de uma string \xE9 basicamente saber quantos\
  \ caracteres ela possui. Programadores fazem isso para validar entradas, delimitar\u2026"
lastmod: '2024-02-25T18:49:44.320602-07:00'
model: gpt-4-1106-preview
summary: "Descobrir o comprimento de uma string \xE9 basicamente saber quantos caracteres\
  \ ela possui. Programadores fazem isso para validar entradas, delimitar\u2026"
title: Descobrindo o comprimento de uma string
---

{{< edit_this_page >}}

## O Que & Porquê?
Descobrir o comprimento de uma string é basicamente saber quantos caracteres ela possui. Programadores fazem isso para validar entradas, delimitar processos ou manipular textos.

## Como Fazer:
O Lua facilita a vida nesse aspecto. Aqui está como você pega esse número:

```Lua
local minha_string = "Olá, Mundo!"
local tamanho = #minha_string -- Uso do operador de tamanho
print(tamanho)  -- A saída será 11
```

E tem mais. Você também pode usar a função `string.len`:

```Lua
local tamanho = string.len(minha_string)
print(tamanho) -- Novamente, a saída será 11
```

## Mergulho Profundo
Historicamente, contar caracteres é tão antigo quanto as primeiras linguagens de programação. No Lua, a facilidade vem do operador `#`, introduzido para dar ao programador rapidez nesse processo, sem recorrer a funções mais verbosas. Uma alternativa é a `string.len`, que faz o mesmo, mas é menos direta. Sobre a implementação, é importante saber que o Lua armazena strings de forma eficiente e conta os caracteres em tempo constante, ou seja, sempre rápido, não importa o tamanho da string.

## Veja Também
- Documentação Oficial do Lua sobre strings: http://www.lua.org/manual/5.4/manual.html#6.4
- Tutorial Lua sobre strings: https://www.tutorialspoint.com/lua/lua_strings.htm
