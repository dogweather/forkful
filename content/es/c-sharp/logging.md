---
title:                "Registro de Actividades en Programación"
date:                  2024-01-26T01:00:24.457971-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Actividades en Programación"
programming_language: "C#"
category:             "C#"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/logging.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?
El registro (logging) es el proceso de grabar eventos de aplicaciones y salida de datos durante el tiempo de ejecución. Los programadores realizan registros para diagnosticar errores, monitorear el rendimiento del software, rastrear acciones de los usuarios y mantener el cumplimiento de estándares de seguridad y empresariales.

## Cómo hacerlo:
En C#, puedes usar el espacio de nombres incorporado `System.Diagnostics` o bibliotecas de terceros como NLog o log4net. Aquí tienes un ejemplo rápido utilizando la interfaz `ILogger` disponible en .NET Core:

```C#
using Microsoft.Extensions.Logging;
using System;

public class Program
{
    public static void Main()
    {
        using var loggerFactory = LoggerFactory.Create(builder => {
            builder.AddConsole();
        });

        ILogger logger = loggerFactory.CreateLogger<Program>();

        logger.LogInformation("Este es un mensaje informativo.");
        logger.LogWarning("Este es un mensaje de advertencia.");
        logger.LogError("Este es un mensaje de error.");
    }
}
```

Salida de muestra:
```
info: Program[0]
      Este es un mensaje informativo.
warn: Program[0]
      Este es un mensaje de advertencia.
fail: Program[0]
      Este es un mensaje de error.
```

## Profundización
La historia del registro en el desarrollo de software es casi tan antigua como la programación misma; ha evolucionado de simples instrucciones de impresión a sistemas sofisticados y configurables. Originalmente, el registro se realizaba escribiendo en archivos o en la consola, pero ha crecido para incluir estructuras más complejas como sistemas de agregación de registros y plataformas de seguimiento distribuido (como la pila ELK o Jaeger).

Alternativas al registro incorporado en .NET incluyen bibliotecas de terceros:
- **NLog**: versátil y fácil de configurar, con muchas características para la ruta, formato y filtrado de registros.
- **log4net**: inspirado en la biblioteca de Java log4j, es altamente configurable desde XML y soporta una variedad de repositorios de registros.

Cuando se trata de detalles de implementación, la elección de tu abstracción de registro (como Microsoft.Extensions.Logging) y el proveedor de registro subyacente pueden afectar significativamente el rendimiento y la fiabilidad de tu aplicación. Es crucial configurar adecuadamente los niveles de registro y asegurar que la escritura de registros no se convierta en un cuello de botella.

Además, el registro estructurado, donde no solo se registran cadenas sino pares clave-valor u objetos, permite registros más precisos y útiles, que son más fáciles de consultar y analizar.

## Consulta también
- [Documentación de Microsoft.Extensions.Logging](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/logging/)
- [Documentación de NLog](https://nlog-project.org/documentation/)
- [Documentación de log4net](https://logging.apache.org/log4net/)
- [Documentación de Serilog](https://serilog.net/) (para un ejemplo de registro estructurado)
