---
date: 2024-01-26 01:11:08.421501-07:00
description: "C\xF3mo hacerlo: Las funciones se hacen m\xE1s complejas, manejando\
  \ varias tareas."
lastmod: '2024-04-05T21:54:00.553922-06:00'
model: gpt-4-1106-preview
summary: "Las funciones se hacen m\xE1s complejas, manejando varias tareas."
title: "Organizando c\xF3digo en funciones"
weight: 18
---

## Cómo hacerlo:
```Lua
-- Definir una función simple para saludar
function greet(name)
    return "Hola, " .. name .. "!"
end

-- Usar la función
print(greet("Programador de Lua")) -- Salida de muestra: Hola, Programador de Lua!
```

Las funciones se hacen más complejas, manejando varias tareas:
```Lua
-- Una función para calcular el área de un rectángulo
function calculateArea(width, height)
    return width * height
end

-- Llamar a la función e imprimir el resultado
local area = calculateArea(5, 4)
print(area)  -- Salida de muestra: 20
```

## Profundización
Lua, desde su creación en los años 90, ha fomentado el diseño modular. Organizar el código con funciones no es único de Lua—ha estado en práctica desde el amanecer de los lenguajes de programación como Fortran y Lisp. Alternativas como el código en línea y el copiar y pegar el mismo código una y otra vez no solo se desaconsejan; son potenciales nidos de errores.

En Lua, las funciones son ciudadanos de primera clase, lo que significa que pueden ser almacenadas en variables, pasadas como argumentos y devueltas por otras funciones. Son versátiles. La naturaleza de un único hilo de ejecución de Lua significa que tienes que mantener las funciones ágiles y eficientes para el rendimiento. Las funciones pueden ser locales (con ámbito) o globales, y entender cuándo usar cada una puede hacer que la eficiencia de tu script sea excelente o se rompa.

## Ver También
- Documentación oficial de Lua sobre funciones: https://www.lua.org/pil/6.html
- Ejemplos prácticos del uso de funciones en Lua: https://lua-users.org/wiki/SampleCode
- Prácticas de código limpio en Lua: https://github.com/Olivine-Labs/lua-style-guide
