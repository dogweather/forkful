---
title:    "Elixir: Escribiendo en el error estándar"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir en la salida de error estándar en Elixir?

La salida de error estándar es una herramienta útil para imprimir mensajes de error y depuración en nuestro código. Al escribir en la salida de error estándar en Elixir, podemos obtener información detallada sobre lo que está sucediendo en nuestro programa y ayudarnos a identificar posibles errores.

## Cómo hacerlo

Para escribir en la salida de error estándar en Elixir, podemos utilizar la función `IO.puts/1` y pasarle el mensaje que deseamos imprimir. Por ejemplo:

```
IO.puts("Este es un mensaje de error")
```

Esto imprimirá el mensaje "Este es un mensaje de error" en la salida de error estándar.

## Inmersión profunda

Escribir en la salida de error estándar en Elixir también nos permite utilizar la función `IO.inspect/2` para imprimir el valor de una variable junto con un mensaje de depuración. Por ejemplo:

```
nombre = "Juan"
IO.inspect(nombre, label: "Mi nombre es: ")
```

Esto imprimirá "Mi nombre es: Juan" en la salida de error estándar.

También podemos usar patrones de concordancia en la función `IO.inspect/2` para imprimir información específica en función de ciertas condiciones. Por ejemplo:

```
n = 5
IO.inspect(n, label: "Valor: ")
```

Esto imprimirá "Valor: 5" en la salida de error estándar. Sin embargo, si modificamos el código para que `n` sea igual a `nil`, obtendremos el siguiente resultado:

```
n = nil
IO.inspect(n, label: "Valor: ")
```

Esto imprimirá "Valor: nil" en la salida de error estándar.

## Vea también

- [Documentación de Elixir sobre IO](https://hexdocs.pm/elixir/master/IO.html)
- [Video de YouTube: Salida de error estándar en Elixir](https://www.youtube.com/watch?v=28tCtCp-x3c)
- [Tutorial de Nivelación Elixir: Imprimir mensajes de depuración en salida de error estándar](https://nivellblog.wordpress.com/2018/10/09/elixir-imprimir-mensajes-de-depuracion-en-salida-de-error-estandar/)