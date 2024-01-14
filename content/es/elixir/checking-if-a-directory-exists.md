---
title:    "Elixir: Comprobando si existe un directorio"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

##Por qué

Antes de hablar de cómo verificar la existencia de un directorio en Elixir, es importante entender por qué querríamos hacerlo. Verificar si un directorio existe puede ser útil al trabajar con archivos y sistemas de archivos. Si el directorio no existe, podremos crearlo antes de realizar cualquier operación de archivo.

##Cómo hacerlo

Para verificar la existencia de un directorio en Elixir, podemos utilizar la biblioteca `File` y su función `exists?`. Primero, necesitamos importar la biblioteca `File` en nuestro archivo de Elixir:

```Elixir
import File
```

Luego, podemos utilizar la función `exists?` y pasarle como argumento la ruta del directorio que queremos verificar:

```Elixir
exists?("ruta/al/directorio")
```

Esta función devolverá `true` si el directorio existe y `false` si no existe.

##Deep Dive

Si queremos profundizar un poco más en cómo funciona esta función, podemos pensar en ella como un atajo para el uso de la función `stat`, que nos devuelve información detallada sobre un archivo o directorio. La función `exists?` simplemente comprueba si hay algún error al intentar obtener esta información a través de `stat`.

Por otro lado, si queremos asegurarnos de que el directorio existe antes de realizar cualquier operación de archivo, podemos utilizar la función `mkdir_p` de la biblioteca `File` para crear el directorio si no existe:

```Elixir
mkdir_p("ruta/al/directorio")
```

##Ver también

- Documentación oficial de la biblioteca `File`: https://hexdocs.pm/elixir/File.html
- Ejemplos de uso de `exists?` en la comunidad: https://elixirforum.com/t/how-to-check-if-a-file-exists/2205
- Otros trucos de Elixir para trabajar con archivos y sistemas de archivos: https://cultivatehq.com/posts/working-with-files-in-elixir/