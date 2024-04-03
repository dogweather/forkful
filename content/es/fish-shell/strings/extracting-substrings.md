---
date: 2024-01-20 17:45:47.010937-07:00
description: "C\xF3mo hacerlo: ."
lastmod: '2024-03-13T22:44:59.489161-06:00'
model: gpt-4-1106-preview
summary: .
title: "Extracci\xF3n de subcadenas"
weight: 6
---

## Cómo hacerlo:
```Fish Shell
# Ejemplo 1: Extraer los primeros 5 caracteres
set cadena "Hola, Mundo!"
echo $cadena[1..5] # Salida: Hola,

# Ejemplo 2: Extraer los últimos 4 caracteres
echo $cadena[-4..-1] # Salida: undo!

# Ejemplo 3: Uso de substrings en condiciones
if test $cadena[1..5] = "Hola,"
    echo "El saludo es 'Hola,'"
end
# Salida: El saludo es 'Hola,'
```

## Profundizando
Las subcadenas son un concepto antiguo en programación, existen desde los inicios de la manipulación de texto. En Fish Shell, a diferencia de otros lenguajes como bash, no necesitas herramientas externas como `cut` o `awk` para trabajos sencillos, aunque permanecen como alternativas potentes para casos complejos. Internamente, Fish utiliza índices basados en la unidad y permite índices negativos para referenciar desde el final hacia atrás, lo cual es más intuitivo para muchos.

## Ver También
- Documentación oficial de Fish Shell sobre [manipulación de strings](https://fishshell.com/docs/current/index.html#syntax-string)
- Tutorial interactivo de Fish Shell para aprender más sobre [cadenas y arreglos](https://fishshell.com/docs/current/tutorial.html#tut_strings_and_arrays)
- Guía de uso de `awk` para manipulación avanzada de texto, mira [AWK](https://www.gnu.org/software/gawk/manual/gawk.html)
