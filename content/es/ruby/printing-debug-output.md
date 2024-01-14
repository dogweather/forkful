---
title:                "Ruby: Imprimiendo salidas de depuración"
simple_title:         "Imprimiendo salidas de depuración"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

Imprimir la salida de depuración es una técnica útil en programación que permite identificar y solucionar problemas en el código. Al mostrar información detallada sobre el estado de nuestro programa en diferentes puntos de ejecución, podemos entender mejor lo que está sucediendo y encontrar dónde se encuentran los errores.

## Cómo hacerlo

Para imprimir la salida de depuración en Ruby, utilizamos el método `puts` seguido de la información que queremos mostrar entre paréntesis. Por ejemplo:

```Ruby
variable = "Hola mundo"
puts "El valor de la variable es: #{variable}"
```

Esto imprimirá la siguiente línea en la consola:

```
El valor de la variable es: Hola mundo
```

También podemos imprimir el valor de una variable sin necesidad de utilizar la interpolación de cadenas, simplemente pasando el nombre de la variable al método `puts`:

```Ruby
variable = "Hola mundo"
puts variable
```

Ambos métodos son útiles para imprimir el valor de una variable en un momento determinado del programa y ver si coincide con lo que esperábamos.

Otra forma de imprimir la salida de depuración es utilizando el método `p`, que también nos mostrará el tipo de dato de la variable además de su valor. Por ejemplo:

```Ruby
numero = 42
p numero
```

La consola imprimirá:

```
42
```

## Deep Dive

Además de mostrar información sobre variables, también podemos usar la salida de depuración para comprobar si se ejecutan ciertas condiciones o bucles. Por ejemplo:

```Ruby
numeros = [1, 2, 3, 4, 5]
numeros.each do |numero|
  if numero.even?
    puts "#{numero} es par"
  else
    puts "#{numero} es impar"
  end
end
```

Este código imprimirá en la consola si cada número del array es par o impar, lo que nos ayuda a verificar si nuestro código funciona correctamente.

También podemos imprimir la salida de depuración a un archivo en lugar de la consola para revisarla más tarde. Para ello, utilizamos el método `File.open` y especificamos el nombre del archivo en el que queremos escribir. Por ejemplo:

```Ruby
File.open("output.txt", "w") do |file|
  file.puts "Esta línea se escribirá en el archivo"
end
```

## Vea también

- [Documentación de Ruby sobre el método `puts`](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-puts)
- [Documentación de Ruby sobre el método `p`](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-p)
- [Artículo sobre depuración en Ruby](https://www.rubyguides.com/2019/09/ruby-debugging/)