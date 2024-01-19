---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Concatenar strings é o processo de juntar duas ou mais strings. Os programadores fazem isso para marcar texto, criar mensagens dinâmicas ou manipular dados.

## Como fazer:

Aqui está um exemplo de como concatenar strings em Lua(versão atual):

```Lua
str1 = "Olá, "
str2 = "mundo!"
str3 = str1 .. str2
print(str3)
```
Saída:
```
Olá, mundo!
```

## Mergulho Profundo

A Lua suporta a concatenação de strings desde seus primeiros dias. Isso foi feito para facilitar a manipulação e formatação de strings com poucos recursos disponíveis.

Uma alternativa à concatenação de strings em Lua seria utilizar a função de formatação `string.format()`. Exemplo:

```Lua
nome = "João"
idade = 24
str = string.format("O Olá, %s! Você tem %d anos.", nome, idade)
print(str)
```
Saída:
```
Olá, João! Você tem 24 anos.
```

A concatenação em Lua é otimizada para garantir o desempenho. No entanto, é importante notar que criar longas cadeias de texto por concatenação contínua pode afetar a performance. É melhor usar a function `table.concat()` quando lidar com muitas strings.

## Veja Também

- Documentação oficial da Lua: [Lua 5.3 Reference Manual](http://www.lua.org/manual/5.3/manual.html)
- Manipulação de string em Lua: [Tutorial do TutorialsPoint](https://www.tutorialspoint.com/lua/lua_strings.htm)
- Guia de Lua detalhado: [Guia de Programação em Lua](https://www.lua.org/pil/1.html)