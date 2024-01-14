---
title:    "Elixir: Escribir un archivo de texto"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Hay muchas razones por las que alguien puede querer escribir un archivo de texto en Elixir. Tal vez estás creando un programa que necesita almacenar datos en un formato legible para humanos. O tal vez estás creando una aplicación web y necesitas generar un archivo HTML para mostrar en el navegador. Independientemente de la razón, saber cómo escribir un archivo de texto en Elixir puede ser una habilidad útil y práctica.

## Cómo hacerlo

En Elixir, puedes escribir un archivo de texto utilizando la función `File.write`. Esta función toma dos argumentos: el nombre del archivo que quieres crear y el contenido que deseas escribir en él. Aquí hay un ejemplo de cómo crear un archivo `ejemplo.txt` y escribir la cadena "¡Hola mundo!" en él:

```elixir
File.write("ejemplo.txt", "¡Hola mundo!")
```

Esta función escribirá el archivo en el directorio actual. Si quieres especificar un directorio diferente, puedes pasar la ruta completa del archivo como primer argumento. Por ejemplo: `File.write("../carpeta/ejemplo.txt", "¡Hola mundo!")`.

Si quieres agregar contenido a un archivo existente en lugar de sobrescribirlo, puedes utilizar la función `File.write!` en su lugar. Ten en cuenta que esto sobrescribirá el contenido existente en el archivo.

Para leer el contenido de un archivo de texto en Elixir, puedes utilizar la función `File.read`. Esta función tomará el nombre del archivo como argumento y devolverá el contenido del archivo como una cadena. Aquí hay un ejemplo:

```elixir
File.read("ejemplo.txt")
```

Esto devolverá la cadena "¡Hola mundo!".

## Inmersión profunda

Cuando estás escribiendo archivos de texto en Elixir, es importante recordar que estás interactuando con un sistema de archivos subyacente. Por lo tanto, es importante tener en cuenta las consideraciones de seguridad al escribir y leer archivos. Por ejemplo, asegúrate de validar y limpiar cualquier entrada del usuario antes de escribirla en un archivo, ya que podrían insertar código malicioso.

También es importante tener en cuenta que la función `File.write` crea un archivo nuevo si el archivo especificado no existe. Si el archivo ya existe, se sobrescribirá sin previo aviso. Para evitar esto, puedes utilizar la función `File.open` en combinación con `IO.write` para escribir en un archivo existente sin sobrescribirlo.

## Ver también

- La documentación oficial de Elixir sobre [File](https://hexdocs.pm/elixir/File.html)
- El blog post de José Valim sobre [Elixir y el sistema de archivos](https://elixir-lang.org/blog/2013/06/23/how-to-work-with-your-file-system-in-elixir/).
- El libro "Programming Elixir" de Dave Thomas, que tiene un capítulo dedicado a trabajar con archivos.