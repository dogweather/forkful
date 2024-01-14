---
title:                "Ruby: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# ¿Por qué crear un archivo temporal en Ruby?

Crear un archivo temporal en Ruby puede ser necesario cuando estamos trabajando con una gran cantidad de datos y necesitamos almacenarlos temporalmente para utilizarlos posteriormente en nuestro código. También puede ser útil para pruebas o para generar archivos que solo necesitaremos por un periodo de tiempo limitado.

## Cómo crear un archivo temporal en Ruby

Para crear un archivo temporal en Ruby podemos utilizar el método `Tempfile.create` que nos permite crear un archivo temporal y escribir en él. Por ejemplo:

```Ruby
require 'tempfile'

temp_file = Tempfile.create('mi_archivo_temporal')

# Escribimos en el archivo temporal
temp_file.puts "Este es un archivo temporal creado en Ruby"

# Cerramos el archivo
temp_file.close
```

Si queremos añadir más contenido al archivo temporal, podemos volver a abrirlo y utilizar el método `puts` nuevamente. Además, podemos especificar una carpeta donde queremos que se cree el archivo temporal utilizando el parámetro `dir` en el método `create`.

## Profundizando en la creación de archivos temporales

Cuando creamos un archivo temporal en Ruby, el sistema operativo nos asigna un nombre único para el archivo y una ruta a donde se encuentra almacenado. Si queremos acceder a esta información, podemos utilizar los métodos `Tempfile#path` y `Tempfile#basename`. Además, podemos especificar algunos parámetros al crear el archivo temporal, como el prefijo del nombre del archivo (`prefix`) y la extensión del mismo (`suffix`).

También es importante tener en cuenta que al finalizar nuestra sesión de Ruby, el archivo temporal se borrará automáticamente. Sin embargo, si queremos asegurarnos de que sea eliminado en un momento específico, podemos utilizar el método `Tempfile#unlink`.

# Ver también

- Documentación oficial de Ruby sobre la creación de archivos temporales: https://ruby-doc.org/stdlib-2.7.1/libdoc/tempfile/rdoc/Tempfile.html
- Tutorial de Ruby para principiantes: https://www.ruby-lang.org/es/documentation/quickstart/