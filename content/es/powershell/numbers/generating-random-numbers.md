---
date: 2024-01-27 20:34:57.189304-07:00
description: "Generar n\xFAmeros aleatorios en PowerShell se trata de crear valores\
  \ num\xE9ricos impredecibles dentro de un rango especificado. Los programadores\
  \ utilizan\u2026"
lastmod: '2024-03-11T00:14:33.106015-06:00'
model: gpt-4-0125-preview
summary: "Generar n\xFAmeros aleatorios en PowerShell se trata de crear valores num\xE9\
  ricos impredecibles dentro de un rango especificado. Los programadores utilizan\u2026"
title: "Generaci\xF3n de n\xFAmeros aleatorios"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Generar números aleatorios en PowerShell se trata de crear valores numéricos impredecibles dentro de un rango especificado. Los programadores utilizan esta capacidad por una miríada de razones, incluyendo pruebas, simulaciones y propósitos de seguridad, donde la imprevisibilidad o imitar el azar del mundo real es crucial.

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
