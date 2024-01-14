---
title:    "Elixir: Imprimiendo salida de depuración"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Por qué imprimir salida de depuración en Elixir?

La impresión de salida de depuración es una técnica utilizada por los desarrolladores para obtener información útil durante la ejecución de un programa. Esto puede ser especialmente útil para identificar y solucionar errores en el código, ya que proporciona una visión detallada de lo que está sucediendo en cada paso.

## Cómo hacerlo

La forma más común de imprimir salida de depuración en Elixir es utilizando la función `IO.inspect/2`, que acepta dos argumentos: el valor que se desea inspeccionar y una cadena opcional que se imprimirá junto con el valor. Veamos un ejemplo:

```Elixir
my_variable = 42
IO.inspect(my_variable, label: "Valor de variable")
```

La salida de esto sería:

```
Valor de variable: 42
```

También se puede utilizar `IO.inspect/2` en expresiones complejas o incluso en bloques de código. Por ejemplo:

```Elixir
IO.inspect(4 + 3 * 2, label: "Resultado de operación")
```

```
Resultado de operación: 10
```

## Profundizando en la impresión de salida de depuración

Además de la función `IO.inspect/2`, Elixir también cuenta con una variedad de herramientas para imprimir salida de depuración más detallada, como `IO.inspect/1` que imprime solamente el valor sin una etiqueta, `IO.inspect/3` que permite personalizar el formato de salida y `IO.inspect/4` que permite especificar a qué archivo y línea de código se refiere la impresión.

Además, Elixir proporciona la macro `require Logger`, que permite imprimir mensajes de registro con diferentes niveles de gravedad, como `:debug`, `:info`, `:warn` y `:error`, lo que permite una mejor organización y filtrado de la salida de depuración.

## Ver también

- [Documentación de Elixir sobre impresión de salida de depuración](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Tutorial de Elixir School sobre el uso de `IO.inspect/2`](https://elixirschool.com/es/lessons/basics/io-inspect/)
- [Artículo de blog sobre el uso de `Logger` en Elixir](https://www.poeticoding.com/depuracion-en-elixir-analizando-el-comportamiento-de-tus-aplicaciones/)