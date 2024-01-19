---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Descobrindo o comprimento de uma string em Lua

## O quê e por quê?

Descobrir o comprimento de uma string é um método comum utilizado para determinar o número de caracteres contidos em dita string. Programadores o fazem para manipular textos, cortar partes, validação de dados e centenas de outras tarefas.

## Como fazer:

No Lua, podemos usar a função `#` para calcular o comprimento de uma string. Veja o exemplo:

```Lua
texto = "Lua é uma linguagem de programação fantástica."
print(#texto)
```

A saída será:

```Lua
45
```

Nesse caso, o número resultante representa o comprimento da string, incluindo espaços e pontuações.

## Mergulhando fundo

PhD Roberto Ierusalimschy, um dos criadores do Lua, fez o projeto para ser simples e eficiente. A função `#` foi introduzida na versão 5.1.

Existem algumas alternativas para calcular o comprimento de uma string, especialmente em versões mais antigas do Lua. Um exemplo comum seria usar um loop para contar os caracteres. No entanto, a função `#` é muito mais rápida e eficiente.

O Lua guarda strings como uma sequência de caracteres na memória, com uma nota do comprimento. Quando chamamos `#texto`, o Lua simplesmente retorna esse valor previamente notado, fazendo desse uma operação muito rápida.

## Veja também

- Para mais detalhes sobre strings em Lua, confira o manual oficial: https://www.lua.org/manual/5.4/manual.html#6.1
- Veja um ótima tutorial sobre cadeias de caracteres (Strings) no Lua na Learn X in Y minutes (Aprenda X em Y minutos): https://learnxinyminutes.com/docs/pt-br/lua-pt.html