---
title:    "Elixir: Escribiendo un archivo de texto"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto en Elixir

Escribir un archivo de texto puede ser útil en muchas situaciones. Puede servir para guardar información importante, compartir datos con otros usuarios o simplemente para tener una copia de seguridad. En Elixir, escribir un archivo de texto es una tarea sencilla y en este artículo te mostraré cómo hacerlo.

## Cómo hacerlo en Elixir

Para escribir un archivo de texto en Elixir, primero debemos abrirlo en modo de escritura utilizando la función `File.open/2`. También debemos especificar la ruta y el nombre del archivo que deseamos crear. Por ejemplo:

```Elixir
File.open("archivo.txt", [:write])
```

Una vez abierto el archivo, podemos utilizar la función `IO.write/2` para escribir nuestro contenido en él. Por ejemplo, si queremos escribir la frase "¡Hola mundo!" en nuestro archivo, podemos hacerlo de la siguiente manera:

```Elixir
IO.write(file, "¡Hola mundo!")
```

Finalmente, debemos cerrar el archivo utilizando la función `File.close/1` para asegurarnos de que todos los cambios se hayan guardado correctamente. Nuestro código completo se vería así:

```Elixir
file = File.open("archivo.txt", [:write])
IO.write(file, "¡Hola mundo!")
File.close(file)
```

## Profundizando en la escritura de archivos de texto en Elixir

Escribir un archivo de texto en Elixir puede parecer una tarea sencilla, pero hay algunas cosas que debemos tener en cuenta para asegurarnos de que todo funcione correctamente. Por ejemplo, si intentamos escribir en un archivo que no hemos abierto previamente, recibiremos un error. Además, también es importante tener en cuenta que si el archivo que estamos intentando crear ya existe, se sobrescribirá con el nuevo contenido.

Otra cosa a tener en cuenta es que podemos utilizar diferentes opciones al abrir un archivo en modo de escritura. Por ejemplo, además de `:write`, también podemos utilizar `:append` para agregar contenido al final del archivo en lugar de sobrescribirlo.

## Ver también

Si estás interesado en aprender más sobre la escritura de archivos de texto en Elixir, aquí tienes algunos enlaces útiles:

- La documentación oficial de Elixir sobre la función `File.open/2`: https://hexdocs.pm/elixir/File.html#open/2
- Un tutorial paso a paso sobre cómo escribir archivos de texto en Elixir: https://elixircasts.io/writing-files-in-elixir
- La documentación oficial de Elixir sobre la función `IO.write/2`: https://hexdocs.pm/elixir/IO.html#write/2