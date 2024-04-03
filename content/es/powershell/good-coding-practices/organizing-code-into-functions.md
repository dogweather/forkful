---
date: 2024-01-26 01:11:29.414973-07:00
description: "Organizar el c\xF3digo en funciones consiste en envolver segmentos de\
  \ c\xF3digo que realizan tareas espec\xEDficas y asignarles un nombre. Se hace para\
  \ hacer el\u2026"
lastmod: '2024-03-13T22:44:59.297686-06:00'
model: gpt-4-1106-preview
summary: "Organizar el c\xF3digo en funciones consiste en envolver segmentos de c\xF3\
  digo que realizan tareas espec\xEDficas y asignarles un nombre."
title: "Organizando c\xF3digo en funciones"
weight: 18
---

## Cómo hacerlo:
Escribamos una función para calcular la suma de dos números. Simple, pero ilustra el punto.

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# Llamar a la función con 5 y 10
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "La suma es $sum"
```

Ejemplo de salida:

```
La suma es 15
```

## Análisis Profundo
Las funciones en PowerShell, como en la mayoría de los lenguajes, no son ninguna novedad. Hemos estado compartimentando código desde los días de Fortran. Se trata de 'no reinventar la rueda'. ¿Alternativas? Claro, scripts o cmdlets. Pero carecen de la pulcritud y la sensibilidad al contexto de las funciones dentro de los scripts.

¿Implementación? Las funciones pueden ser básicas como nuestro ejemplo o complejas con ámbitos, entrada de pipeline y más. Tomemos las `Advanced Functions`. Imitan a los cmdlets con parámetros que tienen atributos, como `[Parameter(Mandatory=$true)]`. Eso es una muestra de la flexibilidad de PowerShell.

## Vea También
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
