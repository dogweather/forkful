---
title:                "Escribir pruebas"
html_title:           "Elixir: Escribir pruebas"
simple_title:         "Escribir pruebas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir pruebas es una práctica común en la programación Elixir. Se trata de crear pruebas automatizadas para comprobar que nuestro código funciona correctamente. Esto nos ayuda a identificar y corregir errores en nuestro código de manera rápida y eficiente.

¿Por qué los programadores escriben pruebas? Porque garantiza que nuestro código funcione correctamente ante cualquier cambio en el mismo. Además, también nos ayuda a documentar el comportamiento esperado de nuestro código y a mejorar la calidad del mismo.

## Cómo hacerlo:

Para escribir pruebas en Elixir, utilizamos el módulo `ExUnit`. Este módulo nos proporciona todas las herramientas necesarias para crear y ejecutar pruebas en nuestro código. Veamos un ejemplo sencillo de cómo escribir una prueba:

```elixir
defmodule Prueba do
  use ExUnit.Case
  # Definimos nuestra prueba
  test "suma de dos números" do
    assert 2 + 2 == 4
  end
end
```

En este ejemplo, estamos definiendo una prueba usando el módulo `ExUnit.Case`. Luego, dentro de nuestra prueba, utilizamos la función `assert` para comprobar que la suma de dos números sea igual a 4. Si la prueba falla, Elixir nos mostrará un mensaje de error indicando en qué parte de nuestra prueba se encuentra el problema. Este es solo un ejemplo simple, pero podemos escribir pruebas más complejas para cubrir diferentes escenarios en nuestro código.

## Profundizando:

Escribir pruebas se ha vuelto una práctica común en la programación moderna. Antes, las pruebas se realizaban de forma manual, lo que consumía mucho tiempo y no garantizaba un resultado preciso. Con las pruebas automatizadas, podemos ahorrar tiempo y verificar el correcto funcionamiento de nuestro código en cualquier momento.

Existen otras alternativas para escribir pruebas, como por ejemplo el popular módulo `RSpec` en Ruby. Sin embargo, en Elixir el uso de `ExUnit` es la opción más común y recomendada.

Si queremos profundizar aún más en las técnicas de escritura de pruebas en Elixir, podemos investigar sobre temas como "Test Driven Development" (TDD) o "Behavior Driven Development" (BDD). Estos enfoques nos ayudarán a escribir pruebas más específicas y a mejorar nuestra metodología de desarrollo.

## Ver también:

- Documentación oficial de ExUnit: https://hexdocs.pm/ex_unit/ExUnit.html

- Tutorial introductorio a la escritura de pruebas en Elixir: https://elixir-lang.org/getting-started/mix-otp/testing-with-exunit.html

- Manual avanzado de Elixir: https://elixir-lang.org/getting-started/mix-otp/tests-and-docs.html#documentation-guidelines