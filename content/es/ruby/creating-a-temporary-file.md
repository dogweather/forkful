---
title:                "Creando un archivo temporal"
html_title:           "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Crear un archivo temporal es la práctica de generar un archivo con contenido que sólo necesitamos por un corto período de tiempo. Los programadores lo hacen para procesar datos de alto volumen, pruebas temporales y para eliminar la necesidad de limpieza manual.

## Cómo:

En Ruby, usamos la biblioteca `Tempfile` para crear archivos temporales. Aquí te enseño cómo hacerlo. 

```Ruby 
require 'tempfile'

temp = Tempfile.new('my_temp')

# Escribe algo en el archivo
temp << "Hola, Mundo!"
temp.close

# Leer desde el archivo 
temp.open
puts temp.read # => "Hola, Mundo!"
temp.close
```

Al cerrar el archivo temporal con `Tempfile#close`, automáticamente se eliminará. Pero si quieres eliminarlo manualmente, puedes usar `Tempfile#unlink`.

## Profundizando

Históricamente, los archivos temporales eran indispensables para superar las limitaciones de memoria. Los datos se escribían en un "archivo temporal" para liberar memoria para otras tareas y luego se leían desde el archivo cuando se necesitaban. 

Una alternativa a la creación de archivos temporales es el uso de flujos de datos en memoria, llamados también "memoria dinámica" o "memoria heap", que algunos idiomas y marcos de trabajo ofrecen. Esto puede ser más rápido, pero también puede consumir valiosos recursos de memoria.

El método `Tempfile#new` en realidad utiliza `File#open` internamente con el parámetro `Tempfile::Remover` para eliminar el archivo cuando se cierra. 

## Ver también:

- Documentación oficial de la biblioteca Tempfile: https://ruby-doc.org/stdlib-2.5.1/libdoc/tempfile/rdoc/Tempfile.html 
- Tutorial más detallado sobre el manejo de archivos en Ruby: https://www.tutorialspoint.com/ruby/ruby_input_output.htm