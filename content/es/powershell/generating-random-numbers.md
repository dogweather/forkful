---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:49:39.130637-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Generar números aleatorios es el acto de producir valores que no tienen un patrón predecible. Los programadores los utilizan para situaciones que requieren elementos de azar o pruebas con datos variados.

## Cómo Hacerlo

Para generar un número aleatorio en PowerShell, emplea el cmdlet `Get-Random`. Así de sencillo, aquí tienes un ejemplo:

```PowerShell
# Genera un número aleatorio entre 0 y 100
Get-Random -Minimum 0 -Maximum 100
```

Y te podría salir algo como esto:

```
58
```

Si necesitas más de un número, usa el parámetro `-Count`:

```PowerShell
# Genera 5 números aleatorios entre 0 y 100
Get-Random -Minimum 0 -Maximum 100 -Count 5
```

Que te daría:

```
28
83
67
45
39
```

## Análisis Profundo

El cmdlet `Get-Random` ha sido parte de PowerShell desde sus primeras versiones. Utiliza un generador de números pseudoaleatorios (PRNG), lo que significa que los números parecen aleatorios, pero se generan a través de un proceso determinista.

Si necesitas una secuencia de números realmente impredecible, podrías considerar otros métodos, como aprovechar las APIs criptográficas de .NET.

```PowerShell
# Utilizar System.Security.Cryptography para generar números aleatorios criptográficamente seguros
[Byte[]]$buffer = New-Object Byte[] 4
[Security.Cryptography.RandomNumberGenerator]::Create().GetBytes($buffer)
[System.BitConverter]::ToInt32($buffer, 0)
```

Es importante notar que mientras `Get-Random` es adecuado para muchos propósitos, no es suficiente para contextos de seguridad donde la aleatoriedad criptográfica es crucial.

## Vea También

- [Get-Random (Microsoft Docs)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random)
- [System.Security.Cryptography.RandomNumberGenerator (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.randomnumbergenerator)
- [Acerca de la aleatoriedad y semillas en la generación de números aleatorios (stackoverflow en inglés)](https://stackoverflow.com/questions/363681/how-do-i-generate-random-integers-within-a-specific-range-in-java)