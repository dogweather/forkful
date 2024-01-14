---
title:                "Ruby: Creando un archivo temporal"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Crear un archivo temporal es una tarea común en la programación que puede ser útil en muchas situaciones. En Ruby, se utiliza el método `Tempfile` para crear archivos temporales en el sistema de archivos. Estos archivos se utilizan para almacenar datos temporales o para realizar operaciones que no requieren una permanencia en el sistema de archivos.

## Cómo hacerlo

Para crear un archivo temporal en Ruby, primero debemos requerir la biblioteca `tempfile`. Luego, podemos usar el método `Tempfile.new` para crear el archivo temporal.

```Ruby
require 'tempfile'
tempfile = Tempfile.new
```

Este código creará un archivo temporal en el sistema con un nombre único y lo almacenará en la variable `tempfile`.

También podemos especificar un prefijo para el nombre del archivo temporal y una dirección donde se almacenará el archivo.

```Ruby
tempfile = Tempfile.new(['prefix', '/path/to/directory/'])
```

Para escribir en el archivo temporal, podemos utilizar el método `write`.

```Ruby
tempfile.write('Hola, mundo!')
```

Podemos leer el contenido del archivo temporal utilizando el método `read`.

```Ruby
puts tempfile.read
=> Hola, mundo!
```

Una vez que hemos terminado de trabajar con el archivo temporal, es importante cerrarlo y eliminarlo del sistema de archivos utilizando el método `close` y `unlink`.

```Ruby
tempfile.close
tempfile.unlink
```

## Profundizando

Los archivos temporales se utilizan frecuentemente en tareas de procesamiento de datos y en aplicaciones web para almacenar archivos de carga temporalmente. En Ruby, también podemos especificar el modo en el que se abre el archivo temporal utilizando el parámetro `mode` en el método `Tempfile.new`.

Por ejemplo, si queremos que el archivo temporal sea de sólo lectura, podemos especificarlo como `mode: 'r'`. También podemos utilizar `mode: 'w+'` para crear un archivo temporal en modo lectura y escritura.

## Ver también

- [Documentación oficial de Tempfile en Ruby](https://ruby-doc.org/stdlib-2.7.2/libdoc/tempfile/rdoc/Tempfile.html)
- [Ejemplos de uso de Tempfile en Ruby](https://www.baeldung.com/ruby-tempfile)
- [Cómo manejar archivos temporales en Ruby on Rails](https://medium.com/platformatec/como-manejar-archivos-temporales-en-ruby-on-rails-ca1d6b5896ec)