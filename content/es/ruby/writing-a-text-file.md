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

## ¿Qué y por qué?
Escribir un archivo de texto en programación simplemente significa guardar información en un formato que pueda ser leído por una computadora. Los programadores utilizan esta técnica para guardar datos como variables, configuraciones o resultados de código.

## ¿Cómo hacerlo?
El proceso de escritura de un archivo de texto en Ruby es sencillo ya que cuenta con un método específico para hacerlo: `File.write()`. Este método requiere dos argumentos: el nombre del archivo y la información que deseamos guardar en él. Veamos un ejemplo práctico:

```Ruby
# Crear un archivo de texto llamado "datos.txt" con la información "Hola mundo!"
File.write("datos.txt", "Hola mundo!")

# Podemos guardar información en una variable y luego escribirla en el archivo
mensaje = "Este es un texto guardado en una variable"
File.write("datos.txt", mensaje)
```

Al ejecutar este código, se creará un archivo llamado "datos.txt" en la misma carpeta que nuestro archivo de código. Si abrimos ese archivo, veremos reflejada la información que guardamos en el método `write()`.

## Inmersión profunda
Escribir archivos de texto es una técnica común en programación y es utilizada para una gran variedad de propósitos. Antes de que existieran las bases de datos, los archivos de texto eran la única forma de guardar y leer información. Aunque actualmente hay alternativas más complejas y avanzadas, muchos programadores siguen utilizando esta técnica por su simplicidad y eficiencia.

Además del método `File.write()`, también podemos utilizar `File.new()` y `File.open()` para crear y abrir archivos de texto respectivamente. Estos métodos tienen algunas diferencias en su implementación, pero también pueden ser utilizados para escribir información en archivos.

## Vea también
- [Documentación de Ruby sobre el método File.write](https://ruby-doc.org/core-3.0.0/File.html#method-c-write)
- [Tutorial de Ruby para escribir y leer archivos de texto](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Página web oficial de Ruby](https://www.ruby-lang.org/es/)