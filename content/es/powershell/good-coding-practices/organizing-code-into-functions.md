---
title:                "Organizando código en funciones"
date:                  2024-01-26T01:11:29.414973-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando código en funciones"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Organizar el código en funciones consiste en envolver segmentos de código que realizan tareas específicas y asignarles un nombre. Se hace para hacer el código reutilizable, legible y mantenible. En lugar de reescribir el mismo código, llama a una función. ¿Quieres solucionar problemas o actualizar? Modifica la función sin tener que examinar pilas de scripts.

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
