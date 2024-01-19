---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué Y Por Qué?
La lectura de argumentos de línea de comandos se refiere a la entrada de información desde la línea de comandos cuando se ejecuta un programa. Los programadores lo hacen para controlar el comportamiento de sus programas, permitiendo opciones y parámetros especificados por el usuario.

## Cómo Se Hace:
A continuación se muestra un ejemplo de cómo leer argumentos de línea de comandos en Ruby:

```Ruby
ARGV.each do|a|
  puts "Argumento: #{a}"
end
```
Si ejecutas `ruby myfile.rb hola mundo`, obtendrás la siguiente salida:

```Ruby
Argumento: hola
Argumento: mundo
```

## Análisis En Profundidad:
La lectura de argumentos de línea de comandos tiene sus raíces en las primeras interfaces de línea de comandos. Los sistemas operativos Unix popularizaron el concepto, empleándolo intensivamente.

En lugar de `ARGV`, puedes usar la biblioteca `OptionParser` para un enfoque más avanzado. Permite gestionar fácilmente opciones cortas y largas, conversiones de tipos y mensajes de ayuda.

```Ruby
require 'optparse'

options = {}

OptionParser.new do |opts|
  opts.on("-a", "--age AGE", Integer) do |age|
    options[:age] = age
  end
end.parse!
```
Este fragmento de código hace que el programa acepte una opción `--age` o `-a` con un valor, que se convertirá a un entero.

## Ver También:
Aquí tienes algunos enlaces de interés para más detalles:

1. [Argumentos de Línea de Comandos en Ruby](https://ruby-doc.org/core-2.6.1/ARGV.html)
2. [Ruby OptionParser](https://ruby-doc.org/stdlib-2.6.1/libdoc/optparse/rdoc/OptionParser.html) 
3. [Historia de la Línea de Comandos](https://www.howtogeek.com/439199/why-was-the-unix-command-line-designed-as-it-was/)

Por último, ¡Asegúrate de practicar la lectura de los argumentos de la línea de comandos! Es una habilidad útil en la caja de herramientas de cada programador Ruby.