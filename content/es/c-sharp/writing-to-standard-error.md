---
title:                "C#: Escribiendo en error estándar."
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida de error estándar puede ser una habilidad muy valiosa para cualquier programador. Esta técnica permite identificar y solucionar problemas en el código de manera más eficiente, lo que ahorra tiempo y esfuerzo en el proceso de depuración.

## Cómo hacerlo

Para escribir a la salida de error estándar en C#, se utiliza el método `Console.Error.WriteLine()`. Este método toma un argumento de tipo `string` que contiene el mensaje a imprimir en la consola de errores.

```C#
Console.Error.WriteLine("Este es un mensaje de error");
```

Este código imprimirá el mensaje "Este es un mensaje de error" en la consola de errores cuando se ejecute.

Otra forma de escribir a la salida de error estándar es utilizando el objeto `StreamWriter`. Para hacerlo, primero se debe crear una instancia de este objeto y luego utilizar su método `WriteLine()` para imprimir el mensaje en la consola de errores.

```C#
StreamWriter errorWriter = new StreamWriter(Console.Error);
errorWriter.WriteLine("Este es otro mensaje de error");
```

Este método también toma un argumento de tipo `string` con el mensaje a imprimir en la consola de errores.

## Profundizando

Además de imprimir mensajes de error en la consola, también es posible redirigir la salida de error estándar a un archivo de texto. Esto puede ser útil cuando se necesita guardar los mensajes de error para su posterior análisis.

Para hacer esto, se utiliza el método `Console.SetError()` para establecer el archivo donde se escribirán los mensajes de error. Luego, se utiliza el método `Console.Error.WriteLine()` para imprimir los mensajes.

```C#
// Establecer el archivo para escribir los mensajes de error
Console.SetError(new StreamWriter("archivo_errores.txt"));

// Imprimir un mensaje de error
Console.Error.WriteLine("Este es un mensaje de error que será guardado en el archivo");

// Cerrar el archivo
Console.Error.Close();
```

Es importante recordar cerrar el archivo después de terminar de usarlo.

## Ver también

- [Documentación de Microsoft sobre la clase Console](https://docs.microsoft.com/es-es/dotnet/api/system.console?view=net-5.0)
- [Tutorial sobre depuración en C#](https://docs.microsoft.com/es-es/visualstudio/debugger/debugging-basics-csharp?view=vs-2019)