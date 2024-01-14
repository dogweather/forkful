---
title:    "Ruby: Imprimiendo salidas de depuración"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por qué

En el mundo de la programación, a veces nos encontramos con problemas difíciles de resolver. En estos casos, imprimir información de depuración (debug output) puede ser una herramienta muy útil. Nos permite ver qué sucede dentro de nuestro código y así poder identificar y corregir errores de manera más eficiente.

## Cómo hacerlo

Para imprimir información de depuración en Ruby, podemos utilizar el método `p` o `puts`. Estos métodos nos permiten imprimir cualquier dato o variable en la consola. Veamos un ejemplo utilizando ambos métodos:

```Ruby
nombre = "María"
edad = 25

p nombre
puts "La edad de " + nombre + " es " + edad.to_s + " años."
```

El código anterior imprimirá lo siguiente en la consola:

```Ruby
"María"
"La edad de María es 25 años."
```

En este caso, utilizamos `p` para imprimir la variable `nombre`, la cual nos devuelve su valor entre comillas ya que es un string. Y con `puts` utilizamos concatenación de strings para imprimir un mensaje más complejo.

Otra opción es utilizar la gema `pry`, la cual nos ayuda a depurar nuestro código de manera más visual y organizada. Para utilizarla, primero debemos instalarla con el comando `gem install pry`. Luego, en nuestro código, podemos requerir la gema y utilizar el método `binding.pry` en el lugar donde queramos detener la ejecución y obtener información sobre nuestros datos y variables.

## Profundizando en la impresión de debug output

Si bien imprimir información de depuración puede ser una herramienta muy útil para solucionar problemas, es importante tener en cuenta que no debe ser utilizado en producción, ya que puede ralentizar el rendimiento del código.

También es importante ser selectivo con lo que se imprime, ya que no queremos sobrecargar la consola con información innecesaria. Por eso, siempre es recomendable revisar cual es la información relevante para solucionar el problema y enfocarse solo en imprimir esa información.

Otra opción para imprimir información de depuración de manera más organizada es utilizar la gema `logger`. Esta nos permite guardar los mensajes de depuración en un archivo de registro (log) en lugar de imprimirlos en la consola, lo cual puede ser especialmente útil en aplicaciones más grandes.

## Ver también

- [Documentación de Ruby: Depuración](https://ruby-doc.org/core-2.6/Kernel.html#method-i-p)
- [Documentación de Pry](https://github.com/pry/pry)
- [Documentación de Logger](https://ruby-doc.org/stdlib-2.6/libdoc/logger/rdoc/Logger.html)