---
date: 2024-01-20 17:56:36.282960-07:00
description: "C\xF3mo Hacerlo: Para leer argumentos en PowerShell, se usa la variable\
  \ autom\xE1tica `$args`, que es un array. Aqu\xED tienes algunos ejemplos pr\xE1\
  cticos."
lastmod: '2024-03-13T22:44:59.307659-06:00'
model: gpt-4-1106-preview
summary: "Para leer argumentos en PowerShell, se usa la variable autom\xE1tica `$args`,\
  \ que es un array."
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

## Cómo Hacerlo:
Para leer argumentos en PowerShell, se usa la variable automática `$args`, que es un array. Aquí tienes algunos ejemplos prácticos:

```PowerShell
# archivo: get-names.ps1
param(
    [string]$firstName,
    [string]$lastName
)

Write-Host "Hola, $firstName $lastName!"
```

Ejecución y salida esperada:
```
PS> .\get-names.ps1 -firstName "Juan" -lastName "Pérez"
Hola, Juan Pérez!
```

También puedes acceder a los argumentos sin declarar parámetros:
```PowerShell
# archivo: greet-user.ps1
Write-Host "¡Hola, $($args[0])!"
```

Ejecución y salida esperada:
```
PS> .\greet-user.ps1 Mundo
¡Hola, Mundo!
```

## Inmersión Profunda:
Históricamente, los argumentos de línea de comandos se utilizan en muchos lenguajes de programación para controlar el comportamiento de las aplicaciones en la ejecución. En PowerShell, `$args` es una variable incorporada que captura todos los argumentos que no están asociados con un parámetro nombrado. Pero, para tener un control más robusto, se utilizan los parámetros, como en el ejemplo con `param(...)`, que también permite la tipificación y la validación de datos.

Alternativamente, puedes usar `Getopts` y `CmdletBinding` para un control más avanzado en scripts que requieren una estructura similar a la de un cmdlet. Además, puedes acceder a `MyInvocation` para más detalles sobre cómo se invocó el script, incluyendo la línea de comandos completa.

Con respecto a la implementación, es importante tener claro que los elementos en `$args` están indexados desde 0, que es lo común en los lenguajes de programación. Sin embargo, no debes olvidar que si un usuario introduce más argumentos de los que espera tu script, necesitarás manejar los adicionales o imprevistos para evitar errores.

## Ver También:
- Guía sobre `$args`: [about_Automatic_Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.2&viewFallbackFrom=powershell-6)
