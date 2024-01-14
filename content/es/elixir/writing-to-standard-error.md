---
title:                "Elixir: Escribiendo a error estándar"
simple_title:         "Escribiendo a error estándar"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir en la salida de error estándar (standard error) es una habilidad útil para cualquier programador de Elixir. Permite imprimir mensajes de error y depuración que pueden ser de gran ayuda en la identificación y solución de problemas en el código.

## Cómo hacerlo

Para escribir en la salida de error estándar, se puede utilizar el módulo `IO` y su función `stderr`. Por ejemplo, si queremos imprimir un mensaje de error en la consola, podemos utilizar el siguiente bloque de código:

```elixir
IO.stderr("¡Error! Este es un mensaje de error.")
```

Esto imprimirá en pantalla el mensaje "¡Error! Este es un mensaje de error." en color rojo, lo que lo diferencia de los mensajes regulares que se imprimen con la función `IO.puts`.

Otra forma de escribir en la salida de error estándar es utilizando la macro `raise` para generar una excepción. En este caso, el mensaje de error se imprimirá automáticamente en la consola al ser lanzada la excepción.

```elixir
raise "¡Oh no! Algo salió mal."
```

## Profundizando

La salida de error estándar es especialmente útil en el desarrollo de aplicaciones en producción. Si un error ocurre en una aplicación en vivo, el mensaje se imprimirá en la consola y puede ser utilizado para identificar el problema y solucionarlo.

Además, es posible personalizar los mensajes de error utilizando el módulo `Kernel` y la función `raise/2`. Esta función toma como argumentos una excepción y un mensaje personalizado que se mostrará en la consola.

```elixir
raise RuntimeError, "¡Hubo un error en la conexión con la base de datos!"
```

## Ver también

Para más información sobre el manejo de errores y excepciones en Elixir, consulta la documentación oficial en [elixir-lang.org](https://elixir-lang.org/getting-started/exceptions.html) y [Elixir School](https://elixirschool.com/es/lessons/basics/errors/).

Si quieres profundizar en el manejo de entradas y salidas en Elixir, te recomiendo leer [este artículo](https://medium.com/@ansart/entendiendo-las-entradas-y-salidas-en-elixir-6cbe7ae4ca4e) sobre el tema.