---
title:                "Imprimindo saída de depuração"
html_title:           "Lua: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que é e Porquê?

Printar saída de debug é o processo de exibir informações e mensagens durante a execução de um programa para facilitar na depuração de possíveis erros e falhas. Programadores utilizam essa técnica para encontrar e corrigir problemas no código de maneira mais eficiente.

## Como fazer:

```
Lua
--[[
Exemplo de printar saída de debug utilizando o comando "print"
--]]
print("Olá, mundo!") -- Saída: "Olá, mundo!"
```

```
Lua
--[[
Exemplo de exibir o valor de uma variável utilizando o comando "print"
--]]
nome = "João"
print("Olá,", nome) -- Saída: "Olá, João"
```

## Mergulho Profundo:

O ato de printar saída de debug existe desde os primórdios da programação, sendo uma técnica essencial para encontrar e resolver problemas em um código. Algumas alternativas para o comando "print" são o uso de breakpoints em um debugger ou a utilização de ferramentas específicas para a depuração de erros de código. A implementação do comando "print" varia de linguagem para linguagem, mas em Lua, ele é um recurso nativo e de fácil utilização.

## Veja também:

- [Apostila de Lua](https://www.caelum.com.br/apostila-lua/)
- [Documentação oficial de Lua](https://www.lua.org/docs.html)
- [Tutorial básico de Lua](https://www.tutorialspoint.com/lua/index.htm)