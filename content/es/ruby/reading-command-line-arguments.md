---
title:                "Ruby: Leyendo argumentos de línea de comando"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Por qué leer argumentos de línea de comando en Ruby

Antes de profundizar en cómo leer argumentos de línea de comando en Ruby, primero debemos discutir por qué es importante y útil hacerlo. 

## Por qué
La lectura de argumentos de línea de comando en un programa Ruby permite al usuario proporcionar información específica al programa en el momento de ejecución. Esto puede ser útil para personalizar la salida del programa o para realizar diferentes acciones basadas en la entrada del usuario.

## Cómo hacerlo
Para leer argumentos de línea de comando en Ruby, utilizaremos el objeto `ARGV`, que contiene un array de todos los argumentos pasados en la línea de comando. Podemos acceder a estos argumentos utilizando índices, empezando por 0. Veamos un ejemplo:

```Ruby
# Este programa imprimirá la palabra pasada como argumento en la línea de comando.
puts "La palabra ingresada fue: #{ARGV[0]}"
```

Si ejecutamos este programa con el comando `ruby argumentos.rb hola`, obtendremos la siguiente salida:

```
La palabra ingresada fue: hola
```

También podemos utilizar un bucle `while` para imprimir todos los argumentos pasados en la línea de comando. Veamos otro ejemplo:

```Ruby
# Este programa imprimirá todos los argumentos ingresados en la línea de comando.
i = 0
while i < ARGV.length
  puts "Argumento ##{i+1}: #{ARGV[i]}"
  i += 1
end
```

Si ejecutamos este programa con el comando `ruby argumentos.rb hola mundo`, obtendremos la siguiente salida:

```
Argumento #1: hola
Argumento #2: mundo
```

## Profundizando
Además de acceder a los argumentos de línea de comando utilizando índices, también podemos utilizar los métodos disponibles en el objeto `ARGV` para realizar diferentes acciones. Algunos de estos métodos incluyen `length`, `include?` y `pop`.

También podemos utilizar la librería `optparse` para crear opciones y argumentos específicos para nuestro programa. Esta librería nos permite crear argumentos opcionales y requeridos, así como proporcionar un mensaje de ayuda para el usuario.

## Ver también
- [Documentación oficial de Ruby sobre ARGV](https://docs.ruby-lang.org/en/2.6.0/ARGF.html)
- [Tutorial de Ruby sobre cómo leer argumentos de línea de comando](https://www.rubyguides.com/2018/11/parameters-argv-gets/)
- [Tutorial de Ruby sobre la librería optparse](https://www.rubyguides.com/2018/12/ruby-optionparser-tutorial/)