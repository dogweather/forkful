---
title:    "C#: Escribiendo en error estándar"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por qué

Escribir registros de error en C# es una forma útil de monitorear y depurar tu código. Al enviar mensajes de error a la consola, puedes identificar rápidamente dónde pueden estar sucediendo errores y tomar medidas para corregirlos.

## Cómo hacerlo

Para escribir en la consola de errores en C#, podemos utilizar el objeto `Console` y su método `Console.Error.WriteLine()`. Este método permite imprimir una cadena de texto en la consola de errores.

```C#
Console.Error.WriteLine("¡Oops! Este es un mensaje de error.");
```

Al ejecutar este código, se imprimirá el mensaje "¡Oops! Este es un mensaje de error." en la consola de errores.

## Profundizando

Además de utilizar `Console.Error.WriteLine()`, también podemos utilizar otras técnicas más avanzadas para escribir en la consola de errores en C#. Por ejemplo, podemos utilizar el objeto `TextWriter` y su método `Error.WriteLine()` para redirigir la salida de error a un archivo en lugar de la consola de errores.

También es importante saber que podemos personalizar los mensajes de error en C#, agregando información adicional como la fecha y hora del error, la ubicación en el código donde ocurrió y más. Esto es especialmente útil al trabajar en proyectos más grandes y complejos.

## Ver También

- [Documentación de Microsoft sobre how writing to standard error works](https://docs.microsoft.com/en-us/dotnet/api/system.console.error)

- [Artículo sobre técnicas avanzadas de depuración en C#](https://www.red-gate.com/simple-talk/dotnet/net-development/debugging-techniques-in-visual-studio/) 

- [Tutorial sobre cómo crear y personalizar mensajes de error en C#](https://exceptionnotfound.net/understanding-c-exception-classes-and-custom-exceptions/)