---
title:                "Leyendo argumentos de línea de comando"
html_title:           "Ruby: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué se utiliza la lectura de argumentos de línea de comando?

La lectura de argumentos de línea de comando es una técnica en la programación que permite a los desarrolladores obtener información ingresada por el usuario en la línea de comando del sistema operativo. Esto puede ser útil para ingresar datos variables o parámetros al programa al momento de su ejecución, en lugar de tener que modificar el código fuente cada vez. Al utilizar la lectura de argumentos de línea de comando, los programadores pueden hacer que sus programas sean más interactivos y versátiles.

## Cómo:

Para leer argumentos de línea de comando en Ruby, podemos utilizar el método `ARGV`. Aquí hay un ejemplo de cómo podemos obtener el primer argumento ingresado por el usuario:

```Ruby
first_arg = ARGV[0]
puts "El primer argumento ingresado fue: #{first_arg}"
```

Si ingresamos el comando `ruby arguments.rb Hola`, el programa imprimirá "El primer argumento ingresado fue: Hola".

También podemos utilizar un bucle `each` para recorrer todos los argumentos ingresados por el usuario y realizar una acción con cada uno de ellos:

```Ruby
ARGV.each do |arg|
  puts "Argumento ingresado: #{arg}"
end
```

Si ejecutamos el comando `ruby arguments.rb Hola Mundo`, el programa imprimirá:
```
Argumento ingresado: Hola
Argumento ingresado: Mundo
```

## Profundizando:

La lectura de argumentos de línea de comando no es una técnica exclusiva de Ruby, es utilizada en muchos otros lenguajes de programación como Python o Java. Sin embargo, cada lenguaje puede tener su propia sintaxis y métodos para leer estos argumentos.

Una alternativa a la lectura de argumentos de línea de comando puede ser la lectura de variables de entorno, que tienen un alcance más amplio y pueden ser accedidas desde cualquier parte del programa.

En Ruby, también es posible utilizar la gem `optparse` para manejar argumentos de línea de comando de una manera más estructurada y fácil de entender.

## Ver también:

- [Documentación de Ruby sobre lectura de argumentos de línea de comando](https://ruby-doc.org/stdlib-2.7.1/libdoc/optparse/rdoc/OptionParser.html)
- [Especificación del estándar POSIX sobre argumentos de línea de comando](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html)
- [Tutorial en español sobre cómo leer argumentos de línea de comando en Ruby](https://ruby-doc.org/docs/ruby-doc-bundle/Tutorial/part_02/argf.html)