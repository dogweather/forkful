---
title:                "Escribiendo en el error estándar"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Escribir en el error estándar permite que tus programas comuniquen problemas y diagnósticos separando estos mensajes del contenido principal de salida. Los programadores hacen esto para facilitar el monitoreo y manejo de errores.

## ¿Cómo Hacerlo?

```C#
using System;

class ErrorStd
{
    static void Main()
    {
        Console.Error.WriteLine("Ha ocurrido un error.");
        
        // Código regular aquí
        Console.WriteLine("Esta es una salida estándar.");
    }
}
```

Salida de muestra:

```
Esta es una salida estándar.
Ha ocurrido un error.
```

Nota que el mensaje de error puede aparecer después en la salida de la consola debido a diferencias en el buffering, pero estarán claramente en flujos separados al redirigirlos en un entorno de producción.

## Profundización

Históricamente, separar la salida estándar de la salida de error ha sido clave en Unix y sistemas similares para el procesamiento y análisis de datos de programas (pipelines). C# y .NET permiten esta tarea a través de `Console.Error`, que escribe en el flujo de error estándar, diferenciándose de `Console.Out` que escribe en la salida estándar. Alternativas incluyen el uso de logging frameworks que proporcionan más control y flexibilidad, como NLog o log4net. Internamente, `Console.Error` usa un `StreamWriter` que escribe en el flujo de error estándar del proceso.

## Ver También

- Documentación de Microsoft sobre `Console.Error`: [https://docs.microsoft.com/en-us/dotnet/api/system.console.error](https://docs.microsoft.com/en-us/dotnet/api/system.console.error)
- Tutorial de Microsoft sobre cómo depurar aplicaciones de C#: [https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-absolute-beginners](https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-absolute-beginners)
- Una introducción a NLog: [https://nlog-project.org](https://nlog-project.org)
- Información sobre log4net: [https://logging.apache.org/log4net/](https://logging.apache.org/log4net/)
