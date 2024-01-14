---
title:                "Elixir: Escribiendo tests"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué

Una parte importante de la programación en Elixir es escribir pruebas (tests) para nuestro código. Estas pruebas nos ayudan a asegurarnos de que nuestro código hace lo que queremos que haga y continua haciendo lo mismo a medida que lo actualizamos. También permiten a otros desarrolladores entender nuestro código y realizar cambios con confianza.

## Cómo hacerlo

Para empezar a escribir pruebas en Elixir, necesitamos utilizar el módulo "ExUnit". Podemos crear un archivo de pruebas con la extensión `.exs` y requerir el módulo ExUnit de la siguiente manera:

```
# importa el módulo ExUnit
import ExUnit

# define un módulo de pruebas
defmodule MiPrueba do

  # especifica que mi módulo usa el módulo ExUnit
  use ExUnit.Case

  # nuestra primer prueba
  test "mi prueba de suma" do
    # assert asegura que 2 + 2 sea igual a 4
    assert 2 + 2 == 4
  end
end
```

Una vez que hemos escrito nuestras pruebas, podemos ejecutarlas utilizando el comando `mix test`. Si todo funciona correctamente, deberíamos ver un resultado similar a esto:

```
Finished in 0.04 seconds
1 test, 0 failures
```

## Profundizando

Existen varias formas de profundizar en la escritura de pruebas en Elixir, incluyendo la utilización de mocks y stubs para simular ciertos comportamientos en nuestras pruebas. También podemos utilizar ExUnit para realizar pruebas de rendimiento y pruebas de propiedades. Es importante explorar estas opciones y descubrir cómo se pueden aplicar a nuestro propio código.

## Ver también

- [Documentación de ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir School - Pruebas y ExUnit](https://elixirschool.com/es/lessons/basics/testing/)
- [Mocking en Elixir usando Mox](https://blog.appsignal.com/2018/07/03/elixir-mocking-with-mox-and-pattern-matching.html)