---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

# Imprimindo Debug Output em Lua

## O que é e por que?

A impressão de debug output é a prática de mostrar dados na tela para rastrear o fluxo do código. Programadores fazem isso para descobrir erros e verificar a lógica do programa.

## Como fazer:

Aqui está um exemplo de como imprimir um debug output em Lua:

```Lua
local variavel = 10
print("Valor da variavel: " .. variavel)
```

A saída será: `Valor da variavel: 10`

E se quiser imprimir várias variáveis ao mesmo tempo:

```Lua
local num1 = 10
local num2 = 20
print("Os números são: " .. num1 .. " e " .. num2)
```

A saída será: `Os números são: 10 e 20`

## Contextualização 

Lua foi originalmente criada em 1993 e logo se tornou uma favorita entre os programadores por sua potência e simplicidade. Desde o início, a função `print()` tem sido um recurso popular para depuração.

Existem alternativas para imprimir um output de debug, como a biblioteca `io`, que proporciona mais controle sobre o output, mas pode ser um pouco mais complicada para iniciantes.

A função `print()` funciona convertendo os argumentos para strings usando a função `tostring()` e os imprimindo de forma formatada.

## Veja também:

* Tutorial de Lua: www.lua.org/pil/
* Referência da função `io`: www.lua.org/manual/5.3/manual.html#6.8
* Tutorial de Debug em Lua: www.tutorialspoint.com/lua/lua_debugging.htm