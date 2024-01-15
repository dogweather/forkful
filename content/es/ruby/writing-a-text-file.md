---
title:                "Escribiendo un archivo de texto"
html_title:           "Ruby: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir un archivo de texto?

Escribir un archivo de texto es una tarea común en la programación en Ruby. Puede ser útil para almacenar y organizar datos, así como para generar informes o registros. Además, escribir un archivo de texto es una forma sencilla de guardar información y compartirla con otros usuarios.

## Cómo escribir un archivo de texto

Para escribir un archivo de texto en Ruby, podemos utilizar el método `File.open` seguido del modo "w" para indicar que queremos escribir en el archivo. Luego, podemos utilizar el método `puts` para añadir líneas de texto al archivo. A continuación, un ejemplo de código:

```Ruby
File.open("mi_archivo.txt", "w") do |file|
  file.puts "Este es mi primer archivo de texto en Ruby."
end
```

El código anterior creará un archivo llamado "mi_archivo.txt" y añadirá la línea "Este es mi primer archivo de texto en Ruby." Para agregar más líneas al archivo, simplemente utilizamos `file.puts` nuevamente. Por ejemplo:

```Ruby
File.open("mi_archivo.txt", "w") do |file|
  file.puts "Este es mi primer archivo de texto en Ruby."
  file.puts "¡Hola a todos!"
end
```

El resultado será un archivo de texto con dos líneas:

```
Este es mi primer archivo de texto en Ruby.
¡Hola a todos!
```

## Profundizando en la escritura de un archivo de texto

Además de utilizar el modo "w" para escribir en un archivo, también podemos utilizar el modo "a" para añadir líneas al final de un archivo existente. Además, podemos especificar la ubicación del archivo utilizando una ruta relativa o absoluta.

Por ejemplo, si queremos escribir en un archivo llamado "nombres.txt" en la carpeta "documentos" en nuestro escritorio, podemos utilizar el siguiente código:

```Ruby
File.open("C:/Usuarios/[Tu Nombre de Usuario]/Escritorio/documentos/nombres.txt", "a") do |file|
  file.puts "Ana"
  file.puts "Juan"
  file.puts "María"
end
```

Este código agregará los nombres "Ana", "Juan" y "María" al final del archivo "nombres.txt" en la carpeta "documentos" en nuestro escritorio. 

## Ver también

- [Guía de formato de texto Markdown en español](https://www.markdownguide.org/es/)
- [Documentación de Ruby sobre el método File.open](https://ruby-doc.org/core-2.6.3/File.html#method-c-open)