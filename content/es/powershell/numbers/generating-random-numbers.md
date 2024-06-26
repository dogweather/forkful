---
date: 2024-01-27 20:34:57.189304-07:00
description: "C\xF3mo hacerlo: PowerShell ofrece un enfoque sencillo para generar\
  \ n\xFAmeros aleatorios utilizando el cmdlet `Get-Random`. Este cmdlet puede producir\
  \ n\xFAmeros\u2026"
lastmod: '2024-03-13T22:44:59.286242-06:00'
model: gpt-4-0125-preview
summary: "PowerShell ofrece un enfoque sencillo para generar n\xFAmeros aleatorios\
  \ utilizando el cmdlet `Get-Random`."
title: "Generaci\xF3n de n\xFAmeros aleatorios"
weight: 12
---

## Cómo hacerlo:
PowerShell ofrece un enfoque sencillo para generar números aleatorios utilizando el cmdlet `Get-Random`. Este cmdlet puede producir números aleatorios dentro de un rango por defecto o un rango especificado.

```PowerShell
# Generar un número aleatorio entre 0 y Int32.MaxValue
$randomNumber = Get-Random
Write-Output $randomNumber
```

Para especificar un rango, usa los parámetros `-Minimum` y `-Maximum`:

```PowerShell
# Generar un número aleatorio entre 1 y 100
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

Para tener más control, puedes instanciar un objeto de la clase `System.Random`:

```PowerShell
# Usando System.Random para una secuencia de números
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

Si necesitas una selección aleatoria de un arreglo o colección, `Get-Random` puede elegir directamente un ítem:

```PowerShell
# Selección aleatoria de un arreglo
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## Análisis Profundo
El cmdlet `Get-Random` en PowerShell aprovecha la clase .NET `System.Random` internamente para generar números pseudoaleatorios. Estos son "pseudo" porque utilizan algoritmos para producir secuencias de números que solo parecen aleatorios. Para la mayoría de las aplicaciones, este nivel de aleatoriedad es suficiente. Sin embargo, para casos de uso que requieren seguridad criptográfica, `System.Random` no es adecuado debido a su naturaleza predecible.

PowerShell y .NET ofrecen `System.Security.Cryptography.RNGCryptoServiceProvider` para la aleatoriedad criptográfica, que es más apropiado para generar llaves de encriptación u otras operaciones sensibles a la seguridad:

```PowerShell
# Números aleatorios criptográficamente seguros
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

Mientras que `Get-Random` y `System.Random` satisfacen un amplio conjunto de necesidades para la aleatoriedad en scripts y lógica de aplicación, es esencial seleccionar la herramienta adecuada para el trabajo, especialmente en aplicaciones centradas en la seguridad donde la previsibilidad puede presentar una vulnerabilidad.
