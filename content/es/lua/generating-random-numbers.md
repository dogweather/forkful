---
title:                "Generando números aleatorios"
html_title:           "Lua: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Generar números aleatorios es una técnica común utilizada en programación para producir valores aleatorios. Los programadores generalmente lo hacen para simular situaciones aleatorias en sus programas o para generar datos aleatorios para pruebas.

## Cómo:
Para generar un número aleatorio en Lua, puedes utilizar la función `math.random()`. Esta función acepta dos argumentos: el número más pequeño y el número más grande que quieres generar. Por ejemplo, si quieres generar un número aleatorio entre 1 y 10, puedes escribir `math.random(1, 10)`.

```Lua
-- Ejemplo simple
print(math.random(1, 10)) 
-- Output: Un número aleatorio entre 1 y 10

-- Ejemplo con un for loop
for i=1, 5 do
    print(math.random(1, 100))
end

-- Output: 
-- Un número aleatorio entre 1 y 100
-- Un número aleatorio entre 1 y 100
-- Un número aleatorio entre 1 y 100
-- Un número aleatorio entre 1 y 100
-- Un número aleatorio entre 1 y 100
```

## Profundizando:
La generación de números aleatorios es una técnica que ha sido utilizada en programación desde sus inicios. Fue introducida en el lenguaje de programación BASIC en 1964 y ha sido una herramienta esencial para los programadores desde entonces.

Además de `math.random()`, existen otras formas de generar números aleatorios en Lua, como utilizar la librería `random` o generar números pseudoaleatorios con semillas. Sin embargo, `math.random()` es la forma más sencilla y comúnmente utilizada.

Cuando se utiliza `math.random()`, es importante tener en cuenta que los números generados no son verdaderamente aleatorios, ya que se basan en una fórmula matemática preestablecida. Sin embargo, para la mayoría de los casos, esto no es un problema y proporcionan un resultado satisfactorio.

## Ver también:
- [Documentación de Lua sobre `math.random()`](https://www.lua.org/manual/5.4/manual.html#6.6) 
- [Documentación de la librería `random` en Lua](https://www.lua.org/pil/20.2.html)
- [Artículo sobre la generación de números aleatorios en programación](https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/)