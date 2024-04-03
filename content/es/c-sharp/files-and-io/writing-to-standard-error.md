---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:39.996785-07:00
description: "C\xF3mo: En C#, escribir en el error est\xE1ndar se puede lograr utilizando\
  \ el flujo `Console.Error`. Este flujo se utiliza espec\xEDficamente para mensajes\
  \ de\u2026"
lastmod: '2024-03-13T22:44:59.093930-06:00'
model: gpt-4-0125-preview
summary: "En C#, escribir en el error est\xE1ndar se puede lograr utilizando el flujo\
  \ `Console.Error`."
title: "Escribiendo en el error est\xE1ndar"
weight: 25
---

## Cómo:
En C#, escribir en el error estándar se puede lograr utilizando el flujo `Console.Error`. Este flujo se utiliza específicamente para mensajes de error y diagnósticos. Aquí hay un ejemplo básico:

```csharp
Console.Error.WriteLine("Error: No se pudo procesar la solicitud.");
```

Salida de ejemplo (a stderr):
```
Error: No se pudo procesar la solicitud.
```

Para escenarios en los que podrías estar utilizando una biblioteca de terceros que ofrece capacidades avanzadas de registro, como `Serilog` o `NLog`, puedes configurar estas bibliotecas para escribir registros de error en stderr. Si bien estos ejemplos se centran en la simple redirección de la consola, recuerda que en aplicaciones de producción, los marcos de registro ofrecen opciones de manejo y salida de errores mucho más robustas. Aquí hay un ejemplo simple con `Serilog`:

Primero, instala el paquete Serilog y su destino de Consola:

```
Install-Package Serilog
Install-Package Serilog.Sinks.Console
```

Luego, configura Serilog para escribir en stderr:

```csharp
using Serilog;

Log.Logger = new LoggerConfiguration()
    .WriteTo.Console(standardErrorFromLevel: Serilog.Events.LogEventLevel.Error)
    .CreateLogger();

Log.Information("Este es un mensaje normal.");
Log.Error("Este es un mensaje de error.");
```

Salida de ejemplo (a stderr para el mensaje de error):
```
[15:04:20 ERR] Este es un mensaje de error.
```

Nota: La configuración `standardErrorFromLevel` en el destino de consola de Serilog redirige todos los eventos de registro en el nivel especificado (Error, en este caso) o superior al flujo de error estándar, mientras que mensajes de nivel inferior como Información se escriben en el flujo de salida estándar.
