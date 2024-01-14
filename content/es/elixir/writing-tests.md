---
title:                "Elixir: Escribiendo Pruebas"
simple_title:         "Escribiendo Pruebas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué escribir pruebas en Elixir

Escribir pruebas es una práctica esencial en el mundo de la programación, ya que nos permite asegurar que nuestro código funciona correctamente y detectar posibles errores antes de que lleguen a producción. En Elixir, esta práctica es aún más importante debido a su fuerte énfasis en la concurrencia y la tolerancia a fallos.

## Cómo escribir pruebas en Elixir

Para escribir pruebas en Elixir, utilizamos el módulo `ExUnit`. Primero, necesitamos definir un módulo de pruebas con el prefijo `Test` y luego utilizar la macro `test` para crear una prueba. Por ejemplo:

```Elixir
defmodule MathTest do
  use ExUnit.Case

  test "sumar números" do
    assert 1 + 1 == 2
  end
end
```

Aquí, estamos definiendo un módulo de pruebas llamado `MathTest` y creando una prueba que verifica que la suma de `1` y `1` es igual a `2`.

Para ejecutar nuestras pruebas, podemos utilizar el comando `mix test` en la terminal. Esto ejecutará todas las pruebas en nuestro proyecto y nos mostrará los resultados.

## Profundizando en la escritura de pruebas

Las pruebas en Elixir pueden ser mucho más complejas que verificar simples operaciones matemáticas. Podemos utilizar patrones de concurrencia y mockear funciones en nuestras pruebas. Además, existen diferentes tipos de aserciones que podemos utilizar para verificar diferentes aspectos de nuestra aplicación.

Es importante tener en cuenta que las pruebas no solo deben enfocarse en verificar que el código funciona, sino también en cubrir diferentes casos y detectar posibles excepciones.

## Ver también

- [Elixir School: Testing](https://elixirschool.com/es/lessons/basics/testing/)
- [Documentación oficial de ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir para todos: Pruebas unitarias con ExUnit](https://elixirparatodos.com/pruebas-unitarias-exunit/)