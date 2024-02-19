---
aliases:
- /es/c-sharp/reading-command-line-arguments/
date: 2024-01-20 17:55:37.612528-07:00
description: "Los argumentos de la l\xEDnea de comandos son datos que pasas a tu aplicaci\xF3\
  n al iniciarla. Los programadores los usan para personalizar la ejecuci\xF3n del\u2026"
lastmod: 2024-02-18 23:09:09.998586
model: gpt-4-1106-preview
summary: "Los argumentos de la l\xEDnea de comandos son datos que pasas a tu aplicaci\xF3\
  n al iniciarla. Los programadores los usan para personalizar la ejecuci\xF3n del\u2026"
title: "Lectura de argumentos de l\xEDnea de comandos"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Los argumentos de la línea de comandos son datos que pasas a tu aplicación al iniciarla. Los programadores los usan para personalizar la ejecución del programa sin cambiar el código fuente.

## Cómo hacerlo:
```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Número de argumentos: " + args.Length);
        foreach (var arg in args)
        {
            Console.WriteLine(arg);
        }
    }
}
```

Si ejecutas `dotnet run hola mundo`, la salida será:

```
Número de argumentos: 2
hola
mundo
```

## Análisis Profundo
El uso de argumentos de la línea de comandos no es nada nuevo; viene desde los días del DOS. C# facilita el acceso a estos argumentos mediante el array `args` del método `Main`. Aparte de `args`, puedes usar `Environment.GetCommandLineArgs()`, que incluye también el nombre del ejecutable como primer elemento. A nivel de implementación, los argumentos se pasan al proceso cuando se invoca y están disponibles a través del sistema operativo.

## Ver También
- Documentación oficial de .NET sobre argumentos de la línea de comandos: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/)
- Guía para argumentos de línea de comandos con .NET Core CLI: [Microsoft Docs - .NET Core CLI](https://docs.microsoft.com/en-us/dotnet/core/tools/)
- Tutorial de C# completo: [Learn C#](https://learn.microsoft.com/en-us/dotnet/csharp/)
