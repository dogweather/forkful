---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:03.916053-07:00
description: "C\xF3mo hacerlo: PowerShell simplifica el proceso de escribir en stderr\
  \ mediante el uso del cmdlet `Write-Error` o dirigiendo la salida al m\xE9todo\u2026"
lastmod: '2024-03-13T22:44:59.308657-06:00'
model: gpt-4-0125-preview
summary: "PowerShell simplifica el proceso de escribir en stderr mediante el uso del\
  \ cmdlet `Write-Error` o dirigiendo la salida al m\xE9todo `$host.ui.WriteErrorLine()`."
title: "Escribiendo en el error est\xE1ndar"
weight: 25
---

## Cómo hacerlo:
PowerShell simplifica el proceso de escribir en stderr mediante el uso del cmdlet `Write-Error` o dirigiendo la salida al método `$host.ui.WriteErrorLine()`. Sin embargo, para la redirección directa de stderr, podrías preferir usar métodos de .NET o la redirección de descriptores de archivos ofrecida por el propio PowerShell.

**Ejemplo 1:** Usando `Write-Error` para escribir un mensaje de error en stderr.

```powershell
Write-Error "Este es un mensaje de error."
```

Salida a stderr:
```
Write-Error: Este es un mensaje de error.
```

**Ejemplo 2:** Usando `$host.ui.WriteErrorLine()` para escribir directamente en stderr.

```powershell
$host.ui.WriteErrorLine("Escritura directa en stderr.")
```

Salida a stderr:
```
Escritura directa en stderr.
```

**Ejemplo 3:** Usando métodos de .NET para escribir en stderr.

```powershell
[Console]::Error.WriteLine("Usando método de .NET para stderr")
```

Salida de este método:
```
Usando método de .NET para stderr
```

**Ejemplo 4:** Redirigiendo la salida de error usando el descriptor de archivo `2>`.

Los descriptores de archivo en PowerShell pueden redirigir diferentes flujos. Para stderr, el descriptor de archivo es `2`. Aquí hay un ejemplo de redirección de stderr a un archivo llamado `error.log` mientras se ejecuta un comando que genera un error.

```powershell
Get-Item ArchivoInexistente.txt 2> error.log
```

Este ejemplo no produce salida de consola, pero genera un archivo `error.log` en el directorio actual que contiene el mensaje de error al intentar acceder a un archivo que no existe.

En conclusión, PowerShell proporciona múltiples métodos para escribir y gestionar eficazmente la salida de error, permitiendo estrategias sofisticadas de manejo de errores y creación de registros en scripts y aplicaciones.
