---
title:                "Escribiendo pruebas"
aliases: - /es/powershell/writing-tests.md
date:                  2024-02-03T19:31:26.328063-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo pruebas"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir pruebas en PowerShell implica crear scripts que validen automáticamente la funcionalidad de tu código de PowerShell, asegurándose de que se comporte como se espera. Los programadores hacen esto para detectar errores temprano, simplificar el mantenimiento del código y asegurar que las modificaciones al código no rompan inadvertidamente la funcionalidad existente.

## Cómo hacerlo:

PowerShell no tiene un marco de pruebas incorporado, pero Pester, un módulo de terceros muy popular, es ampliamente utilizado para escribir y ejecutar pruebas. Así es como comenzar con Pester para probar tus funciones de PowerShell.

Primero, instala Pester si aún no lo has hecho:

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

Luego, considera que tienes una simple función de PowerShell que quieres probar, guardada como `MyFunction.ps1`:

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

Para probar esta función con Pester, crea un script de prueba llamado `MyFunction.Tests.ps1`. En este script, usa los bloques `Describe` e `It` de Pester para definir los casos de prueba:

```powershell
# Importar la función a probar
. .\MyFunction.ps1

Describe "Pruebas de Get-MultipliedNumber" {
    It "Multiplica el número por 2 cuando no se proporciona multiplicador" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "Multiplica correctamente el número por el multiplicador dado" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

Para ejecutar las pruebas, abre PowerShell, navega al directorio que contiene tu script de prueba y utiliza el comando `Invoke-Pester`:

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

La salida de muestra se verá así, indicando si tus pruebas pasaron o fallaron:

```
Starting discovery in 1 files.
Discovery finished in 152ms.
[+] C:\ruta\a\MyFunction.Tests.ps1 204ms (182ms|16ms)
Tests completed in 204ms
Tests Passed: 2, Failed: 0, Skipped: 0 NotRun: 0
```

Esta salida muestra que ambas pruebas han pasado, dándote confianza en que tu función `Get-MultipliedNumber` se comporta como se espera bajo los escenarios que has probado.
