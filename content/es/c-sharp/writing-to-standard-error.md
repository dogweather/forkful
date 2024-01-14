---
title:    "C#: Escribiendo en el error estándar."
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué escribir a la salida estándar de error en C#

Escribir a la salida estándar de error es una herramienta útil para los programadores de C#. Permite mostrar mensajes de error y depurar código de una manera sistemática, lo que facilita la identificación y resolución de problemas en el código.

## Cómo hacerlo

Para escribir a la salida estándar de error en C#, podemos utilizar el objeto "Console" y su método "Error.WriteLine()". Por ejemplo:

```C#
Console.Error.WriteLine("¡Ha ocurrido un error!");
```

Esto imprimirá el mensaje "¡Ha ocurrido un error!" en la consola de error, lo que nos ayudará a identificar dónde se produjo el error en nuestro código.

También podemos utilizar el operador "<<" para escribir a la salida estándar de error. Por ejemplo:

```C#
Console.Error << "El resultado es: " << resultado;
```

En este caso, se imprimirá el mensaje "El resultado es: " seguido del valor de la variable "resultado" en la consola de error.

## Profundizando en la escritura a la salida estándar de error

Además de imprimir mensajes de error, también podemos utilizar la salida estándar de error para mostrar mensajes de depuración o advertencias. Esto puede ser especialmente útil cuando tenemos código complejo y queremos asegurarnos de que está funcionando correctamente.

Otra ventaja de escribir a la salida estándar de error es que podemos redirigir los mensajes a un archivo de registro o a un sistema de gestión de errores. Esto nos permite almacenar y analizar los mensajes de error en un lugar centralizado, lo que facilita la identificación de problemas recurrentes en el código.

## Ver también

- Documentación oficial de C# sobre la clase Console en español: https://docs.microsoft.com/es-es/dotnet/api/system.console?view=net-5.0
- Tutorial sobre la escritura a la salida estándar de error en C#: https://www.tutorialspoint.com/csharp/csharp_error_reporting.htm
- Preguntas frecuentes sobre la salida estándar de error en C#: https://stackoverflow.com/questions/122594/c-sharp-how-to-write-to-the-console-errorstream