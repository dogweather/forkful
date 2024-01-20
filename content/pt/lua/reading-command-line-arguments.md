---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que e por quê?

Lendo argumentos da linha de comando em Lua é uma forma de passar informações adicionais para um programa quando ele é iniciado. É muito útil para alterar o comportamento do programa sem ter que mudar o código.

## Como fazer:

Aqui está um exemplo de como ler argumentos da linha de comando em Lua. Se você executar isso com `lua script.lua arg1 arg2`, verá dois argumentos sendo impressos.

```Lua
-- script.lua
for i, arg in ipairs(arg) do
    print("arg[" .. i .. "]: " .. arg)
end
```

Isso imprimirá:

```
arg[0]: script.lua
arg[1]: arg1
arg[2]: arg2
```

## Mergulho profundo

Historicamente, a leitura de argumentos da linha de comando é um recurso fornecido por muitos sistemas operacionais. Isso permite que os programas sejam mais versáteis em seus usos, com comportamento personalizável de fora.

Lua implementa isso através da tabela global `arg`. Esta tabela contém todos os argumentos passados, com índices de 0 até `#arg`. O índice 0 é sempre o nome do script e os índices positivos são os argumentos passados. Índices negativos contém os argumentos que precedem o nome do script.

Uma alternativa à maneira que Lua lê argumentos da linha de comando pode ser encontrada em bibliotecas como `lapp`, que oferece uma interface mais amigável para definir e ler argumentos.

## Veja também

- [Refêrencia oficial Lua 5.4](https://www.lua.org/manual/5.4/manual.html#6.1)
- [Biblioteca lapp para Lua](https://github.com/stevedonovan/Lapp)