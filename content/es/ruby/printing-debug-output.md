---
title:                "Ruby: Impresión de salida de depuración"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez te has preguntado por qué necesitamos imprimir el resultado de depuración en nuestros programas de Ruby? Aunque pueda parecer una tarea trivial, la impresión de resultados de depuración es una herramienta crucial para entender y solucionar errores en nuestro código.

## Cómo hacerlo
Para imprimir resultados de depuración en Ruby, utilizamos el método `puts`, que nos permite imprimir una cadena de texto hacia la consola. Por ejemplo:

```Ruby
puts "¡Hola Mundo!"
```
El resultado de esta línea de código sería:
```
¡Hola Mundo!
```

Ahora que sabemos cómo imprimir una cadena de texto, podemos utilizarlo para imprimir el contenido de variables y objetos en nuestro programa. Por ejemplo:
```Ruby
nombre = "Juan"
edad = 25
puts "El nombre es #{nombre} y la edad es #{edad} años."
```
El resultado de este código sería:
```
El nombre es Juan y la edad es 25 años.
```

## Profundizando
La impresión de resultados de depuración puede ser útil para entender el flujo de nuestro programa y detectar posibles errores. Sin embargo, también es importante saber cuándo y cómo utilizarla de manera efectiva. Imprimir demasiados resultados de depuración puede sobrecargar la consola y dificultar la lectura del código. Por otro lado, no imprimir suficientes resultados podría ocultar errores potenciales que podrían ser detectados de manera más eficiente con la impresión de resultados de depuración.

Además, si necesitas imprimir resultados de depuración en varias partes de tu código, es posible que desees utilizar una librería o gem específica para ello, como `pry` o `byebug`.

## Ver también
- [Documentación de Ruby sobre el método `puts`](https://ruby-doc.org/core-3.0.0/IO.html#method-i-puts)
- [Tutorial sobre cómo depurar programas en Ruby](https://www.rubyguides.com/2019/06/ruby-debug-environments/)
- [Gem `pry` para imprimir resultados de depuración](https://github.com/pry/pry)
- [Gem `byebug` para imprimir resultados de depuración y hacer debugging interactivo](https://github.com/deivid-rodriguez/byebug)