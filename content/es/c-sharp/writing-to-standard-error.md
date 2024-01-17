---
title:                "Escribir al error estándar"
html_title:           "C#: Escribir al error estándar"
simple_title:         "Escribir al error estándar"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Escribir en la salida de error estándar, o stderr, es una técnica utilizada por los programadores para mostrar mensajes de error en el flujo de datos de un programa. Esto es especialmente útil durante el proceso de depuración del código, ya que permite identificar y resolver errores de manera más eficiente.

## Cómo:
Escribir en stderr en C# es fácil y se puede lograr utilizando la clase `Console`. A continuación se muestra un ejemplo de cómo imprimir un mensaje de error en stderr:

```
Console.Error.WriteLine("¡Error! No se puede procesar el archivo.");
```

La salida de este código sería algo como:

```
¡Error! No se puede procesar el archivo.
```

También puedes utilizar el método `Console.SetError()` para cambiar el destino de la salida de error. Por ejemplo, si deseas redirigir los mensajes de error a un archivo en lugar de la consola, puedes hacer lo siguiente:

```
FileStream fileStream = new FileStream("log.txt", FileMode.OpenOrCreate, FileAccess.Write);
StreamWriter streamWriter = new StreamWriter(fileStream);
Console.SetError(streamWriter);
```

## Profundizando:
La práctica de escribir en stderr proviene del sistema operativo Unix, donde se utilizan diferentes salidas estándar para el flujo de datos y los mensajes de error. En C#, también se puede utilizar la clase `Trace` para escribir en el flujo de error.

Si prefieres utilizar una biblioteca externa, existen varias disponibles en línea que te permiten escribir en stderr en diferentes lenguajes de programación. Algunas de estas bibliotecas también proporcionan funcionalidades adicionales como formateo de texto y registro de errores en un archivo.

En cuanto a la implementación, se recomienda utilizar try-catch-finally para capturar y manejar excepciones en lugar de imprimir directamente en stderr. También es importante tener en cuenta el rendimiento, ya que escribir en stderr en cada iteración de un bucle puede disminuir la velocidad del programa.

## Ver también:
- [Documentación de la clase Console (en inglés)](https://docs.microsoft.com/en-us/dotnet/api/system.console?view=net-5.0)
- [Artículo sobre la manipulación de flujo en C# (en español)](https://www.campusmvp.es/recursos/post/Ciclo-de-instrucciones-try-catch-finally-en-CSharp.aspx)
- [Biblioteca para escribir en stderr en diferentes lenguajes de programación (en inglés)](https://github.com/cppforlife/better-stderr)