---
title:    "Ruby: Imprimiendo salida de depuración"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Por qué imprimir el output de depuración?

Imprimir el output de depuración es una técnica útil para encontrar errores en tu código de Ruby. Al imprimir mensajes de depuración, puedes ver qué valores están siendo asignados a tus variables y cómo fluye la ejecución de tu código, lo que te ayuda a identificar problemas y corregirlos más fácilmente.

## Cómo hacerlo

Para imprimir el output de depuración en Ruby, puedes usar el método `puts()` seguido de la variable o valor que deseas imprimir. Por ejemplo:

```Ruby
nombre = "María"
puts(nombre)
```

Esto imprimirá en la consola el valor asignado a la variable `nombre`, que en este caso sería "María". También puedes imprimir múltiples variables o valores separándolos con comas dentro de los paréntesis de `puts()`, como en el siguiente ejemplo:

```Ruby
nombre = "Juan"
apellido = "García"
puts(nombre, apellido)
```

Esto imprimiría "Juan García" en dos líneas consecutivas.

## Profundizando en la impresión de output de depuración

La impresión de output de depuración también puede ser utilizada para mostrar información adicional sobre el estado de tu código en puntos específicos de tu programa. Por ejemplo, puedes imprimir un mensaje que muestre en qué punto del código se encuentra la ejecución, como en este ejemplo:

```Ruby
puts("Iniciando ejecución del programa...")
```

Esto imprimirá en la consola "Iniciando ejecución del programa..." antes de que se ejecute el resto de tu código.

Otra opción es imprimir mensajes de depuración solo cuando se cumple una determinada condición en tu código. Esto puede ser útil para encontrar errores en secciones específicas de tu programa. Por ejemplo:

```Ruby
contador = 0
while contador < 10 do
  puts("La variable contador es igual a #{contador}.")
  contador += 1
end
```

En este caso, solo se imprimirá el mensaje cada vez que el contador sea menor a 10.

## Ver también

- [Guía de Ruby para principiantes](https://www.ruby-lang.org/es/documentation/quickstart/)
- [Documentación oficial de la librería de Ruby](https://ruby-doc.org/)
- [Ruby on Rails Tutorial en español](https://es.wikibooks.org/wiki/Programaci%C3%B3n_en_ruby_on_rails/Ruby_on_Rails)