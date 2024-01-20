---
title:                "Escritura de un archivo de texto"
html_title:           "Bash: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir un archivo de texto es guardar datos en un archivo legible. Los programadores hacen esto para almacenar configuración, datos de usuario o para lograr la interoperabilidad con otros programas.

## Cómo Hacerlo:
```Ruby
# Primero, abre o crea un archivo llamado 'sample.txt' en modo de escritura
File.open('sample.txt', 'w') do |file|
  # Escribe una línea de texto en el archivo
  file.puts "¡Hola Mundo!"
  # Agrega otra línea
  file.puts "Este es un archivo de texto de ejemplo."
end
```

Salida del archivo `sample.txt`:
```
¡Hola Mundo!
Este es un archivo de texto de ejemplo.
```
## Análisis Profundo
La capacidad de escribir en archivos de texto existe desde los primeros días de la programación. Antes, se usaban tarjetas perforadas y hoy, aunque hay alternativas como las bases de datos, los archivos de texto se mantienen por su simpleza y universalidad. La implementación en Ruby es directa gracias a su clase `File`, que maneja tanto la creación como la escritura de archivos.

## Ver También
- [Clase File en Ruby](https://ruby-doc.org/core/File.html)
- [Tutorial básico de E/S en Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Documentación de API de IO](https://ruby-doc.org/core/IO.html)