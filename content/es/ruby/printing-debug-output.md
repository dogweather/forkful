---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

# Imprimir Debug Output en Ruby: Una Guía Rápida

## ¿Qué y Por Qué?
El debug output es información útil que se muestra mientras se ejecuta un programa. Los programadores lo usan para resolver problemas y entender mejor cómo funciona su código.

## Cómo hacerlo:
Vamos a imprimir "Hola Mundo" en la línea de comando mediante el método `puts`. Es muy fácil. Mira:

```ruby
puts "Hola Mundo"
```

La salida será simplemente:
```
Hola Mundo
```

Ahora, si deseas visualizar el valor de una variable para depurar, utiliza el método `p` para imprimir su valor y su tipo de datos.

```ruby
x = 5
p x
```

La salida será:
```
5
```

¿Y si necesitamos más detalles? En ese caso, podemos usar `pp` (pretty print):

```ruby
require 'pp'

a = [1, 2, 3, ['a', 'b', 'c'], {x: 1, y: 2, z: 3}]
pp a
```

La salida será más legible para objetos complejos:
```
[1, 2, 3, ["a", "b", "c"], {:x=>1, :y=>2, :z=>3}]
```

## Análisis en Profundidad
La salida de depuración tiene sus raíces en los primeros días de la programación, antes de que existieran las herramientas gráficas de depuración. Aunque hay otras maneras de depurar programas en Ruby como crear tus propios `logger` o usar herramientas de depuración gráficas, imprimir la salida de depuración sigue siendo una herramienta valiosa y rápida.

Implementar métodos de impresión de depuración en Ruby es fácil, ya que el lenguaje es rico en métodos de salida incorporados y módulos estándar como el módulo `pp`.

## Ver También
Para más detalles, puedes visitar estos enlaces de la documentación de Ruby:
- [El método de impresión `puts`](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-puts)
- [El método de impresión `p`](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-p)