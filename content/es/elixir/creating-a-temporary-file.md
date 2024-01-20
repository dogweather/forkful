---
title:                "Creando un archivo temporal"
html_title:           "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Creando un archivo temporal es como proyectar un espacio de almacenamiento transitorio para nuestros datos. Los programadores a menudo lo hacen para manejar datos temporales sin traer a riesgo la integridad de los datos permanentes.

## ¿Cómo hacerlo? 

Aquí hay un código básico que muestra cómo crear un archivo temporal y escribir en él.

```Elixir
{:ok, path} = File.touch("mi_archivo_temporal.txt")
File.write!(path, "Hola, este es un archivo temporal.")
IO.puts("El archivo ha sido creado y escrito en: #{path}")
```
Este es el resultado esperado:

```
El archivo ha sido creado y escrito en: mi_archivo_temporal.txt
```

## Análisis en profundidad

* Contexto histórico: La creación de archivos temporales ha sido una práctica común en la programación desde los primeros días de la informática para manejar datos temporales durante una sesión de programación.

* Alternativas: La creación de archivos temporales no es la única forma de manejar los datos temporales. También podemos optar por almacenar estos datos temporalmente en la memoria (RAM) utilizando estructuras de datos temporales.

* Detalles de implementación: En Elixir, utilizamos la función `File.touch` para crear un archivo temporal. Luego, se puede escribir en este archivo utilizando `File.write!`. Si cuenta con permisos adecuados, el archivo temporal se crea y se escribe en su directorio de trabajo actual.

## Ver también

* Documentación oficial de Elixir sobre módulo de archivo: https://hexdocs.pm/elixir/File.html
* Buen tutorial sobre manejo de archivos en Elixir: https://elixirschool.com/es/lessons/specifics/file-io/
* Biblioteca Temp en Elixir para manejo de archivos temporales: https://hexdocs.pm/temp/readme.html