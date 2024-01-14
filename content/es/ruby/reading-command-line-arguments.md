---
title:                "Ruby: Lectura de argumentos de línea de comandos"
simple_title:         "Lectura de argumentos de línea de comandos"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Si estás empezando a aprender Ruby, probablemente te hayas encontrado con el término "argumentos de línea de comandos". Estos son parámetros que se pasan a un programa al momento de ejecutarlo. Aunque puede parecer un concepto complicado al principio, aprender a leer y utilizar los argumentos de línea de comandos en tus programas de Ruby puede ser extremadamente útil y ahorrarte tiempo y esfuerzo en el futuro.

## Cómo hacerlo

Ruby ofrece una serie de métodos y herramientas para leer argumentos de línea de comandos. El método más común es `ARGV`, que recopila todos los argumentos pasados al programa como una matriz. Veamos un ejemplo:

```Ruby
# Código para leer argumentos de línea de comandos
puts "Hola #{ARGV[0]}!" # imprime "Hola" seguido del primer argumento pasado
puts "Tu edad es #{ARGV[1]} años." # imprime "Tu edad es" seguido del segundo argumento pasado
```

Supongamos que ejecutamos este programa con los argumentos `Juan 30` en la línea de comandos, el resultado sería:

```
$ ruby programa.rb Juan 30
Hola Juan!
Tu edad es 30 años.
```

Además de `ARGV`, también puedes utilizar la biblioteca `optparse` de Ruby para leer argumentos de línea de comandos con más precisión. Esta herramienta te permite definir opciones y argumentos específicos que deseas recibir y cómo manejarlos. Aquí hay un ejemplo:

```Ruby
# Código para leer argumentos de línea de comandos usando optparse
require 'optparse'

options = {}

optparse = OptionParser.new do |opts|
  opts.banner = "Uso: programa.rb [opciones]"

  opts.on('-n', '--nombre NOMBRE', 'Indica tu nombre') do |nombre|
    options[:nombre] = nombre
  end

  opts.on('-e', '--edad EDAD', Integer, 'Indica tu edad') do |edad|
    options[:edad] = edad
  end
end

optparse.parse!

puts "Hola #{options[:nombre]}!"
puts "Tu edad es #{options[:edad]} años."
```

Si ejecutamos este programa con los argumentos `-n Juan -e 30`, el resultado sería el mismo que el ejemplo anterior.

## Profundizando

Para aquellos que realmente quieren profundizar en el tema de los argumentos de línea de comandos en Ruby, hay muchas otras herramientas y métodos que puedes utilizar, como `GetoptLong` y `Thor`. También puedes aprender sobre cómo manejar errores y excepciones en los argumentos de línea de comandos.

## Ver también
- [Documentación oficial de Ruby sobre argumentos de línea de comandos](https://ruby-doc.org/core-2.7.1/ARGV.html)
- [Tutorial de Optparse en Ruby](https://www.rubyguides.com/2018/08/ruby-optionparser-class/)
- [Documentación oficial de GetoptLong](https://ruby-doc.org/stdlib-2.7.1/libdoc/getoptlong/rdoc/GetoptLong.html)
- [Documentación oficial de Thor](https://github.com/erikhuda/thor)