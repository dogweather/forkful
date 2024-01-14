---
title:                "Elixir: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

# Por qué

Imagínate que estás escribiendo código en Elixir y te encuentras con un error. ¿Cómo puedes saber qué está causando ese error? Aquí es donde imprimir salidas de depuración en tu código puede ser increíblemente útil. Puedes ver qué valores tienen tus variables en diferentes puntos de tu código y rastrear el origen del error. ¡Sigue leyendo para aprender cómo hacerlo!

# Cómo hacerlo

 Para imprimir salidas de depuración en tu código en Elixir, puedes utilizar la función `IO.inspect/2`. Simplemente pasa la variable que deseas inspeccionar como primer argumento y un mensaje opcional como segundo argumento. Por ejemplo:

```Elixir
age = 25
IO.inspect(age, "Mi edad es:")
```
```Elixir
Mi edad es: 25
```

También puedes usar este método para imprimir el valor de una expresión o una función. Por ejemplo:

```Elixir
IO.inspect(Enum.sum([1, 2, 3]), "La suma es:")
```
```Elixir
La suma es: 6
```
¡Fácil, ¿verdad? Esta es una forma sencilla de imprimir información útil mientras estás depurando tu código.

# Profundizando

Aunque `IO.inspect/2` es una forma rápida y sencilla de imprimir salidas de depuración, puede que no siempre sea la mejor opción. Por ejemplo, si tienes muchas salidas de depuración en tu código, puede ser abrumador y difícil de seguir. En estos casos, puedes utilizar la biblioteca [ex_debug](https://hex.pm/packages/ex_debug) para organizar y filtrar tus salidas de depuración.

Además, si estás trabajando con algún tipo de aplicación web escrita en Elixir, también puedes utilizar la herramienta de depuración [IEx.pry](https://hexdocs.pm/iex/IEx.html#pry/0) para detener la ejecución de tu código y explorar su estado en ese punto.

# Ver también

- [Documentación de Elixir sobre `IO.inspect/2`](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [ex_debug en HexDocs](https://hexdocs.pm/ex_debug/ExDebug.html)
- [Documentación de IEx en HexDocs](https://hexdocs.pm/iex/IEx.html)