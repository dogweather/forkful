---
title:                "Usando una shell interactiva (REPL)"
date:                  2024-01-26T04:16:04.071151-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando una shell interactiva (REPL)"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Qué y Por Qué
REPL significa Bucle de Leer-Evaluar-Imprimir, un entorno interactivo donde puedes probar rápidamente código. Los programadores lo utilizan para experimentar, depurar y aprender las peculiaridades de un lenguaje.

## Cómo hacerlo:
Para saltar al REPL de Lua, simplemente ingresa `lua` en tu terminal. Aquí tienes un ejemplo de sesión:

```Lua
> x = 10
> print(x * 2)
20
> t = {'apple', 'banana', 'cherry'}
> table.insert(t, 'date')
> for i, fruit in ipairs(t) do print(i, fruit) end
1	apple
2	banana
3	cherry
4	date
>
```
En la sesión, declaramos una variable, realizamos aritmética básica, manipulamos una tabla y recorremos sus elementos.

## Inmersión Profunda
La naturaleza ligera de Lua hace que su REPL sea ideal para la creación de prototipos. Ha existido desde la creaciónde Lua a principios de los 90, inspirado por shells interactivos anteriores para lenguajes como Lisp. Alternativas en otros lenguajes incluyen `irb` para Ruby y `python` para Python, cada uno con su propio conjunto de características. El REPL de Lua es minimalista; por lo tanto, puede carecer de características avanzadas que se encuentran en otros, como herramientas de depuración complejas. Para una experiencia más robusta, herramientas como ZeroBrane Studio o LuaDist's LuaRocks ofrecen más que el REPL básico.

## Ver También
- [Manual de Referencia de Lua 5.4 - El Intérprete Independiente de Lua](https://www.lua.org/manual/5.4/manual.html#6)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)