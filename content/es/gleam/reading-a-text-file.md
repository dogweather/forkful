---
title:                "Gleam: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

¡Bienvenidos a mi blog sobre programación en Gleam! Hoy hablaremos sobre cómo leer un archivo de texto en Gleam y por qué es una habilidad útil para cualquier programador.

## ¿Por qué?

La lectura de archivos de texto es un elemento básico en el mundo de la programación. Puede ser necesario para leer datos de entrada o para manipular archivos de texto como parte de una aplicación. Saber cómo leer un archivo de texto te ayudará a ser un programador más completo y eficiente.

## Cómo hacerlo

Para leer un archivo de texto en Gleam, utilizamos la función `file.read` y le pasamos el nombre del archivo como argumento. Aquí tienes un ejemplo que lee un archivo llamado "datos.txt" y lo imprime por consola:

```Gleam
let archivo = "datos.txt"
let contenido = file.read(archivo)
IO.print(contenido)
```

Al ejecutar este código, deberías ver el contenido del archivo impreso en la consola. ¡Así de fácil es leer un archivo de texto en Gleam!

## Profundizando

Para entender mejor cómo funciona la lectura de archivos en Gleam, es importante conocer el tipo de dato que se devuelve al leer un archivo. La función `file.read` devuelve un tipo `File.ReadResult` que contiene dos posibles valores: `File.ReadSuccess` si la lectura del archivo fue exitosa y `File.ReadError` si hubo un problema al leer el archivo.

Es importante manejar ambas posibilidades al leer un archivo para asegurarnos de que nuestro código funcione correctamente. Puedes leer más sobre el manejo de errores en la [documentación de Gleam](https://gleam.run/book/tour/error_handling.html).

## Ver también

Si quieres seguir aprendiendo sobre la lectura de archivos de texto en Gleam, te recomiendo revisar estos recursos:

- [Documentación oficial de Gleam sobre lectura de archivos](https://gleam.run/book/libraries/file.html#File)
- [Tutorial en video sobre lectura de archivos en Gleam](https://www.youtube.com/watch?v=37U7AtTBdkQ)
- [Ejemplo de código para leer un archivo de texto en Gleam](https://gist.github.com/gleam-lang/b21cc29a2e599e659985b16b4dc621d5)

¡Espero que este artículo te haya sido útil y te motive a seguir explorando las posibilidades de Gleam! ¡Hasta la próxima!