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

##Por qué: Imprimir salidas de depuración en Elixir

¿Alguna vez te has encontrado atascado en un problema de programación en Elixir y no sabes por dónde empezar a buscar? Una forma sencilla de entender lo que está ocurriendo en tu código es utilizando la función `IO.inspect/2` para imprimir la salida de depuración. Esto te permitirá ver el valor de las variables en diferentes puntos de tu programa y solucionar más fácilmente cualquier error.

##Cómo: Ejemplos de código y salida de muestra

####Código de ejemplo 1
```Elixir
defmodule Calculadora do
  def sumar(a, b) do
    IO.puts "Sumando #{a} y #{b}" # salidas de depuración
    a + b
  end
end

Calculadora.sumar(2, 3)
```

####Salida de muestra 1
```
Sumando 2 y 3
5
```

En este ejemplo, utilizamos `IO.puts` para imprimir una salida de depuración en forma de cadena antes de sumar dos números. Si ejecutamos este código, veremos la salida de depuración en nuestra consola antes del resultado final. Esto nos permite entender cómo se está ejecutando nuestro código y qué valores están siendo utilizados en cada paso.

También podemos utilizar `IO.inspect/2` para imprimir la salida de depuración en forma de estructuras de datos más complejas, como listas, mapas o tuplas.

####Código de ejemplo 2
```Elixir
defmodule Lista do
  def crear_lista do
    lista = [1, 2, 3]
    IO.inspect lista # salida de depuración
    lista
  end
end

Lista.crear_lista()
```

####Salida de muestra 2
```
[1, 2, 3]
[1, 2, 3]
```

En este ejemplo, utilizamos `IO.inspect` para imprimir la lista creada en nuestro programa. Esto nos permite comprobar que la lista se ha creado correctamente antes de devolverla como resultado.

Puedes utilizar estas funciones de salida de depuración en diferentes partes de tu código para entender cómo se están manipulando los datos y solucionar cualquier error o problema que encuentres.

##Profundizando: Más información sobre la salida de depuración

Existen otras funciones más avanzadas para imprimir salidas de depuración en Elixir, como `IO.inspect/2` con modificadores especiales para mostrar más información sobre las estructuras de datos o `Logger.debug/1` para registrar salidas de depuración en un archivo de registro.

Estas funciones pueden ser útiles en situaciones más complejas de depuración, como en el caso de aplicaciones web o en la integración con otras herramientas de depuración.

Mantener una buena práctica de utilizar salidas de depuración regularmente en tu código puede ahorrarte mucho tiempo y esfuerzo en la solución de problemas.

## Ver también

- [Documentación oficial de Elixir para `IO.inspect/2`](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Documentación oficial de Elixir para `Logger.debug/1`](https://hexdocs.pm/logger/Logger.html#debug/1)
- [Artículo sobre técnicas de depuración en Elixir](https://medium.com/@lasseebert/debugging-elixir-techniques-1e30ad71fee3)