---
title:                "Creando un archivo temporal"
html_title:           "Ruby: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez te has preguntado por qué necesitas crear archivos temporales en tus programas de Ruby? Bueno, en esta breve guía te explicaremos por qué es útil y necesario crear archivos temporales en tu código.

## Cómo hacerlo
La creación de archivos temporales en Ruby es bastante sencilla gracias al uso del módulo `Tempfile` integrado en la biblioteca estándar. Este módulo nos permite crear, abrir, escribir y cerrar archivos temporales de manera eficiente y segura.

Para crear un archivo temporal, simplemente debemos utilizar el método `Tempfile.new` y pasarle como argumento el nombre que deseamos darle al archivo. Por ejemplo:

```Ruby
temp_file = Tempfile.new('mi_tempfile') # crea un archivo temporal llamado "mi_tempfile"
```

Una vez que hemos creado nuestro archivo temporal, podemos escribir en él utilizando el método `write`. Por ejemplo:

```Ruby
temp_file.write('Este es un ejemplo de texto que se escribirá en mi archivo temporal.')
```

Podemos verificar si nuestro archivo temporal existe utilizando el método `exist?` y también podemos obtener su ruta utilizando el método `path`.

```Ruby
temp_file.exist? # devuelve true si existe el archivo temporal
temp_file.path # devuelve la ruta completa del archivo temporal
```

Finalmente, cuando hemos terminado de utilizar nuestro archivo temporal, debemos cerrarlo utilizando el método `close` y luego podemos eliminarlo utilizando el método `unlink`. Por ejemplo:

```Ruby
temp_file.close # cierra el archivo temporal
temp_file.unlink # elimina el archivo temporal
```

Es importante seguir este proceso ya que los archivos temporales no se eliminan automáticamente y pueden acumularse en nuestro sistema si no los manejamos correctamente.

## Deep Dive
Además de la funcionalidad básica de crear, escribir y eliminar archivos temporales, el módulo `Tempfile` también nos permite especificar la ubicación y el modo de apertura del archivo temporal.

Para especificar la ubicación, podemos pasarle una ruta como segundo argumento al método `Tempfile.new`. Por ejemplo:

```Ruby
temp_file = Tempfile.new('mi_tempfile', '/ruta/de/destino') # crea un archivo temporal en la ubicación especificada
```

También podemos especificar el modo de apertura utilizando el parámetro `mode`. Por defecto, se utiliza el modo de apertura `w+`, que permite crear un archivo y escribir en él, pero también podemos utilizar otros modos como `r` para sólo lectura o `a` para añadir contenido al archivo existente.

```Ruby
temp_file = Tempfile.new('mi_tempfile', mode: 'r') # crea un archivo temporal en modo de sólo lectura
```

Además, el módulo `Tempfile` nos ofrece métodos adicionales como `chmod` para cambiar los permisos del archivo temporal, `binmode` para indicar que el archivo contiene datos binarios o `rewind` para volver al principio del archivo.

¡Con todo esto ya estás listo para utilizar archivos temporales en tus programas de Ruby!

## See Also
- [Documentación oficial del módulo Tempfile en Ruby](https://ruby-doc.org/stdlib-2.7.0/libdoc/tempfile/rdoc/Tempfile.html)
- [Tutorial de Tempfile en Ruby de Codecademy](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-files/cheatsheet)