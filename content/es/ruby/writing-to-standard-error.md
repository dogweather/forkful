---
title:                "Ruby: Escribiendo a error estándar"
simple_title:         "Escribiendo a error estándar"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir a la salida estándar en Ruby?

La salida estándar, también conocida como `STDOUT`, es una herramienta fundamental en cualquier programa de Ruby. Permite mostrar información importante al usuario, como mensajes de error o resultados de operaciones, de una manera clara y legible. Aprender a escribir a la salida estándar es esencial para cualquier programador en Ruby.

## Cómo escribir a la salida estándar en Ruby

Escribir a la salida estándar en Ruby es sencillo y práctico. Usamos el método `puts` para imprimir mensajes en la pantalla:

```Ruby
puts "¡Hola mundo!"
```

Este código imprimirá en la pantalla el mensaje "¡Hola mundo!".

También podemos utilizar el método `print` para imprimir en la salida estándar sin agregar un salto de línea al final:

```Ruby
print "¡Hola "
print "mundo!"
```

Este código imprimirá "¡Hola mundo!" sin un salto de línea entre las dos palabras.

Otro método útil es `p`, que nos permite imprimir el valor de una variable junto con su tipo de dato:

```Ruby
nombre = "Juan"
p nombre    # Output: "Juan"
```

## Profundizando en la escritura a la salida estándar en Ruby

A veces, es necesario escribir mensajes de error o información de depuración en la salida estándar. Para hacerlo, podemos utilizar el objeto `$stderr`, que nos permite escribir directamente a la salida de error.

Fíjate en este ejemplo:

```Ruby
def dividir(a, b)
    return a / b
rescue ZeroDivisionError => error
    $stderr.puts "¡No se puede dividir entre 0!"
    $stderr.puts error.message
end

dividir(10, 0)
```

El código anterior producirá el siguiente resultado en pantalla:

```
¡No se puede dividir entre 0!
divided by 0
```

Este es solo un ejemplo de cómo podemos utilizar la salida de error para manejar situaciones inesperadas en nuestros programas.

## Ver también

- [Métodos de salida estándar en Ruby](https://ruby-doc.org/core-2.7.2/Kernel.html#method-i-puts)
- [Manejo de errores en Ruby](https://www.rubyguides.com/2019/04/ruby-exceptions/)
- [Documentación de Ruby sobre stderr](https://ruby-doc.org/core-2.7.2/IO.html#method-c-new-label-Standard+Streams)

¡Ahora estás listo para escribir a la salida estándar en tus programas de Ruby! Recuerda siempre verificar la documentación oficial y explorar diferentes métodos para encontrar la mejor forma de imprimir información para tus usuarios. ¡Buena suerte!