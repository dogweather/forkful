---
title:                "Escribiendo pruebas"
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Escribir tests es crear código que verifica que tu código hace lo que esperas. Los programadores escriben tests para prevenir errores, facilitar la actualización de código y mejorar la calidad del software.

## Cómo hacerlo:
En Elixir, usamos ExUnit para escribir tests. Instálalo añadiendo `{:ex_unit, "~> 1.6", only: :test}` a tus dependencias en `mix.exs` y ejecuta `mix test` para correr tus pruebas. Aquí tienes un ejemplo:

```elixir
defmodule MathTest do
  use ExUnit.Case
  doctest Math

  test "sums two numbers" do
    assert Math.add(1, 2) == 3
  end
end
```

Resultado de ejecución:

```
.

Finished in 0.04 seconds
1 test, 0 failures
```

## Deep Dive
Los tests en Elixir datan desde el inicio del lenguaje, inherente con el framework ExUnit. Alternativas como ESpec existen, aportando un estilo más de RSpec. Para testear hay que entender las aserciones (`assert`), las refutaciones (`refute`) y cómo organizar el código con `setup` y `context`. Mocks se usan con moderación en Elixir por su naturaleza funcional.

## Ver También
* [Documentación de ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)
* Guía [Getting Started](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html) de Elixir
* ESpec para un framework de testing al estilo RSpec: [https://github.com/antonmi/espec](https://github.com/antonmi/espec)
