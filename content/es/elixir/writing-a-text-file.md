---
title:                "Elixir: Creando un archivo de texto"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Por qué escribir un archivo de texto en Elixir

Escribir un archivo de texto es una parte fundamental de la programación en cualquier lenguaje de programación. En Elixir, los archivos de texto son una forma de almacenar y manipular datos de manera fácil y sencilla. En este artículo, exploraremos por qué deberías aprender a escribir archivos de texto en Elixir y cómo puedes hacerlo.

## Cómo hacerlo

Para escribir un archivo de texto en Elixir, primero debes abrir un nuevo archivo utilizando la función `File.open/2`. Esta función acepta dos argumentos: el nombre del archivo y una lista de opciones. Dentro del bloque de `File.open/2`, puedes utilizar la función `IO.write/2` para escribir en el archivo. Por ejemplo:

```Elixir
File.open("mi_archivo.txt", [:write, :utf8], fn(file) ->
  IO.write(file, "¡Hola, mundo!")
  end)
```

Este código creará un archivo llamado "mi_archivo.txt" y escribirá la cadena "¡Hola, mundo!" dentro de él.

## Profundizando

Escribir un archivo de texto en Elixir es una tarea sencilla, pero hay algunas cosas que debes tener en cuenta. Por ejemplo, el segundo argumento de `File.open/2` es una lista de opciones que determina cómo se abrirá el archivo. Puedes utilizar diferentes opciones como `:append` para agregar texto al final del archivo o `:read` para leer el contenido del archivo. Además, es importante asegurarse de cerrar el archivo utilizando la función `File.close/1` una vez que hayas terminado de escribir en él.

## Ver también

Si quieres aprender más sobre cómo escribir archivos de texto en Elixir, aquí tienes algunos enlaces útiles:

- [La documentación oficial de Elixir sobre archivos de texto](https://hexdocs.pm/elixir/File.html)
- [Un artículo en español sobre cómo escribir archivos de texto en Elixir](https://www.elixir-lang.org/getting-started/basic-operators.html)
- [Un repositorio de GitHub con ejemplos de código de escritura de archivos en Elixir](https://github.com/elixir-lang/elixir/search?q=file&type=code)

¡Ahora estás listo para comenzar a escribir archivos de texto en Elixir! ¡Hasta la próxima!