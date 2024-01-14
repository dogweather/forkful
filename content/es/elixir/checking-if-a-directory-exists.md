---
title:    "Elixir: Verificando si existe un directorio"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué
Comprobar si un directorio existe es una tarea muy común en el desarrollo de aplicaciones en Elixir. Esto es especialmente importante cuando se trabaja con archivos y es necesario asegurarse de que el directorio donde se guardarán o leerán los archivos realmente exista.

## Cómo
Para comprobar si un directorio existe en Elixir, podemos utilizar la función `File.exists?/1` que nos permite verificar si un archivo o directorio existe en una ubicación específica.

```elixir
File.exists?("my_directory")
```

Esta función devuelve un booleano, verdadero `true` si el directorio existe y falso `false` si no existe.

Podemos incluso utilizar esta función para comprobar si un archivo específico existe en un directorio específico.

```elixir
File.exists?("my_directory/my_file.txt")
```

Si el archivo existe, la función devolverá verdadero `true` y si no existe, devolverá falso `false`.

## Profundizando
Además de la función `File.exists?/1`, también contamos con la función `File.dir?/1` que nos permite verificar si un directorio específico existe en una ubicación determinada. Esta función devuelve verdadero `true` si el directorio existe y si es un directorio, y falso `false` si no existe o si es un archivo.

```elixir
File.dir?("my_directory")
```

También podemos utilizar la función `System.cwd/0` para obtener el directorio de trabajo actual y luego utilizar la función `File.exists?/1` para verificar si existe un directorio específico en esa ubicación.

```elixir
current_dir = System.cwd()
File.exists?(current_dir <> "/my_directory")
```

## Ver también
- [Elixir Docs - File](https://hexdocs.pm/elixir/File.html)
- [Elixir Docs - System](https://hexdocs.pm/elixir/System.html)