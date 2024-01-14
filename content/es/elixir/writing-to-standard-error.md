---
title:    "Elixir: Escribiendo en el error estándar"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida de error estándar es una práctica común en la programación en Elixir. Esta técnica permite a los desarrolladores mostrar mensajes de error o información de depuración en la consola mientras ejecutan sus programas.

## Cómo hacerlo

Para escribir a la salida de error estándar en Elixir, se puede utilizar la función `IO.write/1` y pasar como argumento el mensaje que se desea mostrar. También se puede utilizar la función `IO.puts/1` para escribir el mensaje seguido de una nueva línea.

```Elixir
IO.write("Este es un mensaje de error")
IO.puts("Este es un mensaje de información")
```

El resultado en la consola sería:

```Elixir
Este es un mensaje de error

Este es un mensaje de información
:ok
```

## Profundizando

En Elixir, también se puede utilizar la macro `IO.inspect/1` para imprimir el valor de una variable junto con su tipo de datos en la salida de error estándar. Esta es una forma útil de realizar una depuración rápida de variables en una función o módulo.

```Elixir
name = "Juan"
IO.inspect(name)
```

El resultado en la consola sería:

```Elixir
"Juan": String
:ok
```

Es importante tener en cuenta que al utilizar cualquiera de estas funciones para escribir a la salida de error estándar, el programa no se detendrá en caso de un error. Para lograr esto, se puede utilizar la función `IO.inspect/2` y pasar como segundo argumento la opción `suffixed: true`. Esto imprimirá el mensaje de error y luego detendrá la ejecución del programa.

## Ver también

- [Documentación de Elixir sobre la salida estándar](https://hexdocs.pm/elixir/IO.html#write/2)
- [Tutorial de Elixir en español](https://elixirschool.com/es/lessons/basics/io/)

¡Esperamos que este artículo te haya sido útil para comprender cómo escribir a la salida de error estándar en Elixir! Recuerda que esta es una herramienta útil para depurar tus programas y entender mejor lo que está sucediendo en tu código. ¡Feliz programación!