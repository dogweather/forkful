---
title:                "Imprimiendo salida de depuración"
html_title:           "Elixir: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Imprimir mensajes de depuración o debug output es una técnica utilizada por programadores para mostrar información relevante durante la ejecución de un programa. Esto es especialmente útil para identificar errores o entender qué está sucediendo en el código.

## Cómo hacerlo:

En Elixir, podemos imprimir mensajes de depuración utilizando la función ```IO.inspect``` seguida de la variable o valor que queremos imprimir. Por ejemplo:

```Elixir
IO.inspect "Hola"
```

Esto imprimirá "Hola" en la consola. También podemos imprimir valores en una estructura de datos, como una lista o un mapa:

```Elixir
IO.inspect ["rojo", "azul", "verde"]
```

Esto imprimirá la lista completa. Además, podemos usar patrones de coincidencia para imprimir solo ciertos valores:

```Elixir
IO.inspect [name: "Juan", age: 30, location: "Madrid"], [:name, :age]
```

Esto imprimirá solo el nombre y la edad en lugar de toda la estructura de datos.

## Profundizando:

La impresión de mensajes de depuración no es una técnica nueva, ya que ha sido utilizada por programadores desde los primeros días de la programación. Sin embargo, es importante usarla con moderación y solo cuando sea necesario, ya que puede hacer que el código se vuelva más difícil de leer y mantener.

Además, existen otras técnicas de depuración más avanzadas, como el uso de herramientas específicas para seguimiento de errores. Sin embargo, la impresión de mensajes de depuración sigue siendo una herramienta útil para entender qué está sucediendo en el código en tiempo real.

En Elixir, también podemos usar el módulo ```Logger``` para imprimir mensajes de depuración, lo que nos permite controlar el nivel de detalle y el formato del mensaje.

## Ver también:

- [Documentación oficial de Elixir sobre depuración](https://elixir-lang.org/getting-started/debugging.html)
- [Tutorial de depuración en Elixir](https://dockyard.com/blog/2018/06/28/troubleshooting-in-elixir-using-binaries-pattern-matching-and-IO-inspect)
- [Librería Pry para Ruby, una herramienta de depuración poderosa](https://pryrepl.org/)