---
title:                "Capitalizando uma string"
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Capitalizar uma string significa transformar todas as letras iniciais de palavras em maiúsculas. Programadores fazem isso para padronizar textos, como títulos ou nomes próprios, melhorando a legibilidade e a apresentação dos dados.

## Como Fazer:
Para capitalizar uma string em Lua, você pode utilizar a função `gsub` encontrada na biblioteca padrão de string. Aqui está um exemplo simples:

```Lua
function capitalizarString(str)
  return (str:gsub("(%a)([%w_']*)", function(first, rest) return first:upper()..rest:lower() end))
end

print(capitalizarString("olá, mundo do lua!"))  -- Saída: Olá, Mundo Do Lua!
```

## Mergulho Profundo:
O método `gsub` é usado para substituir ocorrências em strings. Desde o surgimento do Lua em 1993, a capitalização de strings sempre foi realizada por meio de funções personalizadas, pois Lua não fornece uma função integrada para isso. Alternativas incluem o uso de `string.lower` e `string.upper` para manipular a string em mais detalhes ou o emprego de expressões regulares em ambientes que ofereçam essa funcionalidade, como o LuaJIT.

A capitalização de uma string deve considerar também casos especiais, como abreviações e nomes que não seguem a regra padrão. Outra consideração é o desempenho, uma vez que, a função `gsub` pode ser menos eficiente se usada repetidamente em grandes volumes de texto.

## Veja Também:
- Documentação oficial do Lua `string` library: [http://www.lua.org/manual/5.4/manual.html#6.4](http://www.lua.org/manual/5.4/manual.html#6.4)
- Tutorial LuaStrings na lua-users wiki: [http://lua-users.org/wiki/LuaStrings](http://lua-users.org/wiki/LuaStrings)
- LuaJIT, uma implementação Just-In-Time Compiler do Lua que oferece expressões regulares: [http://luajit.org/](http://luajit.org/)
