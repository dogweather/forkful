---
title:    "Elixir: Creando un archivo temporal"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por qué
La creación de archivos temporales es una tarea común en la programación, especialmente cuando se trabaja con aplicaciones distribuidas o con múltiples procesos corriendo simultáneamente. Los archivos temporales pueden ser utilizados para almacenar datos temporales o para compartir información entre diferentes procesos.

## Cómo
Para crear un archivo temporal en Elixir, podemos utilizar la función `temp_file/1` del módulo `File`:

```Elixir
{path, file} = File.temp_file("my_prefix")
```

Esta función devuelve una tupla con dos valores: la ruta al archivo temporal y su nombre. El parámetro opcional "my_prefix" se utiliza para crear un prefijo en el nombre del archivo.

También podemos especificar la extensión del archivo temporal utilizando el parámetro `suffix`:

```Elixir
{path, file} = File.temp_file("my_prefix", ".txt")
```

Una vez que hemos creado nuestro archivo temporal, podemos escribir datos en él utilizando la función `write/2` y leer los datos con `read/1`:

```Elixir
File.write(file, "¡Hola Mundo!")
content = File.read(file)
IO.puts(content) # output: ¡Hola Mundo!
```

Finalmente, cuando hayamos terminado de utilizar el archivo temporal, podemos borrarlo con la función `delete/1`:

```Elixir
File.delete(file)
```

## Deep Dive
Detrás de escena, la función `temp_file` utiliza la librería `:os` de Erlang para crear el archivo temporal. También podemos utilizar la función `:os.tmpname/1` directamente desde Elixir para crear un archivo temporal. Sin embargo, es importante tener en cuenta que esta función devuelve solo el nombre del archivo, no la ruta completa.

En algunos casos, puede ser necesario controlar la ubicación donde se crea el archivo temporal. Para ello, podemos utilizar la función `:os.mkstemp/2` de Erlang, que nos permite especificar la ruta y el prefijo del archivo.

## Ver también
- [Documentación de Elixir sobre la función `temp_file`](https://hexdocs.pm/elixir/File.html#temp_file/1)
- [Documentación de Erlang sobre la función `:os.tmpname/1`](http://erlang.org/doc/man/os.html#tmpname-1)
- [Documentación de Erlang sobre la función `:os.mkstemp/2`](http://erlang.org/doc/man/os.html#mkstemp-2)