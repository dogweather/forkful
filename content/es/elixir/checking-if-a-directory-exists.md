---
title:                "Elixir: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

En programación, es importante verificar si una carpeta existe antes de realizar ciertas acciones en ella, como crear nuevos archivos o mover archivos existentes. Esto puede evitar errores y asegurarse de que nuestro código se ejecute correctamente.

## Cómo hacerlo

```Elixir
# Verificar si una carpeta existe en una ruta dada
File.dir?("/ruta/a/carpeta")
#=> true

# Verificar si una carpeta existe en una ruta relativa
File.dir?("carpeta")
#=> true

# Comprobar si una carpeta existe en una ruta especificada por una tupla
File.dir?({:home, "mi_usuario", "Documentos"})
#=> true
```

## Profundizando

Elixir proporciona la función `File.dir?` para comprobar si una carpeta existe en una ruta especificada. Esta función devuelve un booleano `true` si la carpeta existe y `false` si no existe. Además, esta función también se puede utilizar para comprobar si una carpeta existe en una ruta relativa, lo que significa que solo se necesita el nombre de la carpeta en lugar de la ruta completa.

Una forma avanzada de utilizar `File.dir?` es proporcionando una tupla como argumento. Esta tupla debe contener la ruta absoluta de la carpeta que se desea verificar, pero también puede contener opciones adicionales como el nombre de usuario o el directorio raíz. Al proporcionar una tupla, podemos verificar si una carpeta existe en una ubicación específica del sistema de archivos.

## Ver también

- [Documentación de Elixir sobre File.dir?](https://hexdocs.pm/elixir/File.html#dir?/1)
- [Artículo en inglés sobre cómo verificar si una carpeta existe en Elixir](https://elixirschool.com/en/lessons/basics/basics/#exists-questionmark)