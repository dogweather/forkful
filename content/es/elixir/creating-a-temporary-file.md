---
title:    "Elixir: Creando un archivo temporal"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Crear un archivo temporal puede ser una herramienta muy útil en la programación en Elixir. Puede ser usado para almacenar datos temporales, como un cache, o para ejecutar código que no necesite ser guardado permanentemente. Además, es una práctica común al trabajar con aplicaciones web, especialmente cuando se necesita manipular archivos en el sistema.

## Cómo

Para crear un archivo temporal en Elixir, primero debemos importar el módulo `File` y utilizar la función `tempfile/1`. Por ejemplo:

```Elixir
import File

{:ok, file} = tempfile("mi_archivo")
```

Aquí, estamos utilizando la función `tempfile/1` para crear un archivo temporal con el nombre "mi_archivo" y asignándolo a la variable `file`. La función devuelve una tupla con el átomo `:ok` y la ruta al archivo creado. También podemos especificar una ruta o prefijo específico utilizando la opción `:dir` o `:prefix` respectivamente.

Una vez que tenemos nuestro archivo temporal, podemos escribir en él utilizando las funciones `write/2` o `write!/2` y leer su contenido utilizando `read/1` o `read!/1`. También podemos usar la opción `:encoding` para especificar la codificación del archivo.

```Elixir
{:ok,file} = tempfile("mi_archivo.txt", prefix: "temp_", dir: "/tmp")

# escribiendo en el archivo
write(file, "Este es un texto de prueba")
# leyendo el contenido del archivo
{:ok, contenido} = read(file)

contenido # "Este es un texto de prueba"

# cerrando el archivo
{:ok,_} = File.close(file)
```

También podemos borrar el archivo temporal con `delete/1`, lo que nos puede ahorrar espacio en el sistema después de usarlo.

```Elixir
{:ok, file} = tempfile("mi_archivo.txt")

delete(file) # el archivo se borra automáticamente
```

## Profundizando

Además de las opciones mencionadas anteriormente, la función `tempfile/1` también permite algunas opciones más avanzadas como `:permissions` para especificar permisos del archivo, `:binary` para indicar si el archivo debe ser tratado como binario o `:name` para especificar el nombre completo del archivo temporal. Para más información, se puede consultar la documentación oficial de Elixir.

## Veamos También

- [Documentación oficial de Elixir sobre creación de archivos temporales](https://hexdocs.pm/elixir/File.html#tempfile/1)
- [Artículo sobre manejo de archivos en Elixir](https://www.what-could-possibly-go-wrong.com/everything-you-need-to-know-about-file-io-in-elixir/) (en inglés)
- [Guía práctica de Elixir sobre interacción con el sistema de archivos](https://elixir-lang.org/getting-started/file-system.html) (en inglés)