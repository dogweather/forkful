---
date: 2024-01-26 00:56:00.667284-07:00
description: "C\xF3mo hacerlo: PowerShell ha evolucionado mucho desde su inicio como\
  \ Monad. El manejo de errores se volvi\xF3 m\xE1s robusto con el tiempo, ofreciendo\u2026"
lastmod: '2024-04-05T21:54:00.644009-06:00'
model: gpt-4-1106-preview
summary: PowerShell ha evolucionado mucho desde su inicio como Monad.
title: Manejo de errores
weight: 16
---

## Cómo hacerlo:
```PowerShell
# Try-Catch básico para manejar excepciones
try {
    # Código que podría desencadenar un error
    $result = 1 / 0
} catch {
    # Qué hacer si ocurrió un error
    Write-Host "Vaya, ha ocurrido un error: $_"
}

# Mostrando un mensaje de error personalizado
try {
    Get-Item "nonexistentfile.txt" -ErrorAction Stop
} catch {
    Write-Host "No se pudo encontrar el archivo."
}

# Usando la variable $Error para inspeccionar el último error
```

## Análisis Profundo
PowerShell ha evolucionado mucho desde su inicio como Monad. El manejo de errores se volvió más robusto con el tiempo, ofreciendo características similares a otros lenguajes de programación. La sintaxis `try-catch-finally` es una de esas influencias cruzadas de lenguajes como C#. Antes de ella, los programadores se basaban mucho en verificar condiciones y usar la variable automática `$Error`.

PowerShell también tiene dos tipos principales de errores: terminadores y no terminadores. Los errores terminadores detendrán el script a menos que sean capturados en un bloque `try-catch`, mientras que los no terminadores no lo harán a menos que especifiques `-ErrorAction Stop`. Esta distinción es crucial ya que otorga un control fino sobre el manejo de errores, decidiendo si un error realmente justifica detener todo el script o simplemente puede ser registrado e ignorado.

El manejo de errores de PowerShell permite también un bloque `finally`, que se ejecuta pase lo que pase - ocurra un error o no. Es excelente para tareas de limpieza.

Cuando estés profundamente involucrado en las trincheras de la escritura de scripts, también puedes manejar tipos específicos de excepciones, dándote un control aún más fino.

Alternativamente, está el viejo parámetro `-ErrorVariable` para capturar errores sin lanzar una excepción. Y la variable `$?` te dice si la última operación fue exitosa. Son herramientas útiles, aunque un poco menos limpias que un sólido `try-catch`.

## Ver también
- [about_Try_Catch_Finally](https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
