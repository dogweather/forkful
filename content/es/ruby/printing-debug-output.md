---
title:                "Impresión de salida de depuración"
html_title:           "Ruby: Impresión de salida de depuración"
simple_title:         "Impresión de salida de depuración"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Imprimir mensajes de depuración puede ser útil en la programación para identificar errores y entender cómo funciona el código. Esto es especialmente importante en Ruby, donde existen múltiples formas de imprimir mensajes de depuración.

## ¿Cómo hacerlo?

Para imprimir mensajes de depuración en Ruby, se pueden utilizar los métodos `p`, `puts` o `print` seguidos del texto o variable que se desea imprimir. Por ejemplo:

```Ruby
p "Hola mundo!"
# Output: "Hola mundo!"

num = 5
puts "El número es: #{num}"
# Output: El número es: 5

print "El cuadrado de #{num} es: "
print num**2
# Output: El cuadrado de 5 es: 25
```

También se pueden utilizar estos métodos para imprimir el contenido de un arreglo o hash:

```Ruby
array = [1, 2, 3]
p array
# Output: [1, 2, 3]

hash = {nombre: "Ana", edad: 30}
puts "Mi nombre es #{hash[:nombre]} y tengo #{hash[:edad]} años."
# Output: Mi nombre es Ana y tengo 30 años.
```

Es importante tener en cuenta que `p` imprime el valor literal de una variable, mientras que `puts` y `print` lo convierten a una cadena. Por lo tanto, `p` es especialmente útil para imprimir el valor de una variable booleana o un objeto.

## Deep Dive

Además de utilizar los métodos mencionados anteriormente, también se puede utilizar la herramienta `pp` (pretty print) para imprimir objetos complejos de manera más legible. Por ejemplo:

```Ruby
array = [1, 2, {nombre: "Juan", edad: 25}]
pp array
# Output: [1, 2, {:nombre=>"Juan", :edad=>25}]
```

En caso de necesitar imprimir mensajes de depuración en una parte específica del codigo, se pueden utilizar las gemas `pry` o `byebug`, las cuales permiten detener la ejecución del código y acceder al contexto actual para inspeccionar variables y objetos.

## Ver también

- [Blog sobre impresión de mensajes de depuración en Ruby](https://blog.honeybadger.io/ruby-debugging-magic-cheat-sheet/)
- [Documentación oficial de Ruby para el método `p`](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-p)
- [Documentación oficial de Ruby para el método `pp`](https://ruby-doc.org/stdlib-2.7.1/libdoc/pp/rdoc/PP.html)