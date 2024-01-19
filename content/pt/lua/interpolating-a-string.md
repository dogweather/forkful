---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que é & Por quê?

A interpolação de strings em Lua é o processo de substituir placeholders por valores específicos dentro de uma string. Programadores fazem isso para incorporar valores de variáveis ou expressões diretamente em strings, facilitando a leitura e compreensão do código.

## Como fazer:

Segue um exemplo de código em Lua para interpolação de strings usando a função `string.format`.

```Lua
local nome = "João"
local idade = 25
local interpolação = string.format("Ola, meu nome é %s e eu tenho %d anos.", nome, idade)
print(interpolação)
```

A saída desse código será:

```
Ola, meu nome é João e eu tenho 25 anos.
```

## Um mergulho profundo:

Historicamente, Lua nunca teve uma função de interpolação de string embutida. Era comum concatenar strings ou usar a função `string.format`. Em termos de alternativas modernas, você pode usar bibliotecas externas como `Penlight` que fornece uma função `printf`. 

Em termos de implementação, o `string.format` em Lua foi baseado no `printf` da linguagem C. Os placeholders começam com o simbolo "%" e terminam com um caractere de conversão que indica o tipo da variável a ser substituída, como "s" para strings e "d" para números inteiros.

## Veja também:

- Lua 5.3 Reference Manual: https://www.lua.org/manual/5.3/
- Lua-users wiki (String Recipes): http://lua-users.org/wiki/StringRecipes
- Penlight Documentation: https://stevedonovan.github.io/Penlight/api/index.html