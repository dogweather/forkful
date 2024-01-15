---
title:                "Escribiendo en el error estándar"
html_title:           "C#: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has encontrado con algún error en tu código y no sabes por dónde empezar a solucionarlo? Es ahí donde escribir a la salida estándar de error (stderr) puede ser una gran herramienta para el desarrollo de tu programa en C#.

## Cómo hacerlo

Para escribir a la salida estándar de error en C#, puedes hacer uso de la clase `Console` y su método `Error.WriteLine()`. Este método acepta como parámetro una cadena de texto que será mostrada en la pantalla de error.

```C#
Console.Error.WriteLine("¡Ups! Ha ocurrido un error en tu código.");
```

El resultado de este código sería un mensaje de error que se mostrará en la consola, junto con cualquier otra información del error que se haya programado.

```
¡Ups! Ha ocurrido un error en tu código.
```

## Profundizando

Escribir a la salida estándar de error en C# puede ser muy útil para depurar y encontrar errores en tu código. Sin embargo, es importante tener en cuenta que esta salida no está diseñada para ser utilizada regularmente en la ejecución de tu programa, ya que puede afectar su rendimiento.

Es importante también mencionar que, a diferencia de la salida estándar (stdout), la salida estándar de error (stderr) no se puede redirigir. Esto significa que no puedes enviar la salida de error a un archivo o a otra aplicación, sino que siempre se mostrará en la consola.

## Ver también

- [Documentación de Microsoft sobre la clase Console](https://docs.microsoft.com/es-es/dotnet/api/system.console?view=net-5.0)
- [Artículo sobre cómo manejar errores en C#](https://platzi.com/tutoriales/1378-control-de-errores-en-c-net-por-que-y-como-hacerlo/)