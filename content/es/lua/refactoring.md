---
title:                "Refactorización"
date:                  2024-01-26T01:45:42.902855-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactorización"

category:             "Lua"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/refactoring.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Refactorizar es el arte de ajustar código existente para mejorar su estructura, legibilidad y eficiencia sin alterar su comportamiento externo. Los programadores lo hacen para hacer su código más mantenible, reducir la complejidad y, a menudo, como un paso preliminar antes de agregar nuevas características o corregir errores.

## Cómo hacerlo:
Tomemos una función simple de Lua y la refactorizamos. Comenzamos con una función que calcula la suma de números en una lista pero está escrita sin mucha consideración por la eficiencia o claridad:

```Lua
function sumList(numbers)
    local result = 0
    for i=1, #numbers do
        for j=1, #numbers do
            if i == j then
                result = result + numbers[i]
            end
        end
    end
    return result
end

print(sumList({1, 2, 3, 4})) -- Salida: 10
```

Refactorizar a una versión más eficiente y legible:
```Lua
function sumListRefactored(numbers)
    local result = 0
    for _, value in ipairs(numbers) do
        result = result + value
    end
    return result
end

print(sumListRefactored({1, 2, 3, 4})) -- Todavía salida: 10
```

La versión refactorizada elimina el bucle interno redundante, utilizando `ipairs` para iterar a través de la lista de manera limpia.

## Análisis Profundo
Históricamente, la refactorización proviene de la comunidad de programación de Smalltalk a finales de los 80 y fue popularizada por el libro de Martin Fowler 'Refactoring: Improving the Design of Existing Code'. En Lua, refactorizar a menudo implica simplificar condicionales complejos, desglosar funciones grandes en más pequeñas y optimizar el uso de tablas para mejorar el rendimiento.

Refactorizar en Lua tiene sus advertencias; la naturaleza dinámica de Lua y la tipificación flexible pueden hacer que ciertas refactorizaciones, como renombrar variables o cambiar firmas de funciones, sean más arriesgadas si no se hacen con precaución. Herramientas para el análisis estático de código (como `luacheck`) pueden reducir tales riesgos. Alternativas incluyen el desarrollo guiado por pruebas (TDD), donde el código se refactoriza continuamente como una parte integral del proceso de desarrollo, en contraste con una fase de refactorización separada.

## Ver También
- "Programming in Lua" de Roberto Ierusalimschy para mejores prácticas y ejemplos.
- "Refactoring: Improving the Design of Existing Code" de Martin Fowler para principios aplicables a través de lenguajes.
- Directorio LuaRocks (https://luarocks.org/) para herramientas y módulos destinados a mantener y refactorizar el código de Lua.
