---
title:                "C#: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías escribir en el estándar de error?

Escribir en el estándar de error es una técnica importante en la programación de C#, ya que permite mostrar información importante o mensajes de error al usuario durante la ejecución de un programa. Además, ayuda en la depuración de código y a identificar posibles problemas en el mismo.

## Cómo escribir en el estándar de error

Para escribir en el estándar de error en C#, se utiliza el objeto `Console` y su método `Error.WriteLine()`. Este método recibe como parámetro una cadena de texto que se imprimirá en la salida de error.

```C#
//Ejemplo de escritura en el estándar de error
Console.Error.WriteLine("¡Ha ocurrido un error! Por favor, revisa tu código.");
```

El resultado de este código sería:

```
¡Ha ocurrido un error! Por favor, revisa tu código.
```

## Profundizando en la escritura en el estándar de error

Además de simplemente imprimir un mensaje de error, se pueden realizar otras acciones con el estándar de error en C#. Por ejemplo, se puede utilizar el objeto `TextWriter` y su método `StandardError`, que permite redirigir la salida de error a un archivo de texto.

```C#
//Ejemplo de redirección del estándar de error a un archivo
var file = new StreamWriter("error.txt"); //crea un archivo error.txt
Console.SetError(file); //seleciona el archivo como salida de error
Console.Error.WriteLine("Este mensaje estará en el archivo error.txt");
```

Además, también se pueden utilizar diferentes métodos de formato para mostrar información más específica en la salida de error, como el método `Error.Write()` que no agrega un salto de línea al final de la cadena de texto.

```C#
//Ejemplo de escritura en el estándar de error sin salto de línea
Console.Error.Write("Número de error: ");
Console.Error.WriteLine(404);
```

El resultado sería:

```
Número de error: 404
```

## Ver también
- [Documentación de Microsoft sobre el objeto Console en C#](https://docs.microsoft.com/es-es/dotnet/api/system.console?view=netcore-3.1)
- [Tutorial de programación de C# en español](https://www.tutorialesprogramacionya.com/csharpya/index.php?inicio=0)
- [Blog de programación en C# en español](http://csharplearning.blogspot.com/)

¡Espero que este artículo te haya sido útil para aprender sobre la escritura en el estándar de error en C#! ¡Sigue investigando y mejorando tus habilidades de programación!