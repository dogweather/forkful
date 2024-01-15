---
title:                "Escribiendo pruebas"
html_title:           "Elixir: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir pruebas en Elixir?

Las pruebas son una parte importante del desarrollo de software, ya que nos permiten asegurarnos de que nuestro código funciona correctamente y mantiene su funcionalidad a medida que se realizan cambios. En Elixir, esto es especialmente importante debido a su naturaleza de lenguaje funcional y la concurrencia que ofrece. Escribir pruebas nos ayuda a tener un código más robusto y confiable.

## Cómo escribir pruebas en Elixir

Para escribir pruebas en Elixir, utilizamos el módulo `ExUnit`, que viene incluido en la librería estándar del lenguaje. Este módulo nos proporciona funciones y macros que nos permiten definir y ejecutar pruebas de manera sencilla.

Para comenzar, debemos crear un archivo de pruebas con la extensión `.exs`, por ejemplo, `mi_prueba.exs`. En este archivo, importamos `ExUnit` y luego definimos nuestra prueba utilizando la macro `test`. Dentro de esta macro, utilizamos diferentes assertivas para asegurarnos de que nuestro código se comporte como se espera.

Veamos un ejemplo de cómo probar una función que suma dos números:

```Elixir
defmodule MiPrueba do
  use ExUnit.Case
  test "suma dos numeros" do    assert sumar(2, 3) == 5  end
end
```

En este ejemplo, definimos un módulo de prueba llamado `MiPrueba` y dentro de él, usamos la macro `test` para definir nuestra prueba. Dentro de esta macro, utilizamos `assert` para verificar que la función `sumar` devuelve el resultado esperado.

Para ejecutar nuestras pruebas, podemos utilizar el comando `mix test` en la terminal y obtener un resultado como el siguiente:

```
ExUnit.run: 1 failed, 1 passed, 0 skipped

Failed tests:

  * MiPrueba.suma dos numeros
    * assert sumar(2, 3) == 5

Passed tests:

  * MiPrueba.otra prueba
    * assert "hola" == "hola"
```

Como se puede ver, nuestra prueba falló porque la función `sumar` no está definida. Pero una vez que la definimos y la ejecutamos nuevamente, obtendremos un resultado exitoso.

## Deep Dive: Más información sobre escribir pruebas en Elixir

Elixir también nos ofrece la posibilidad de utilizar la herramienta `ExCoveralls` para generar un reporte de cobertura de nuestras pruebas. Esto nos permite visualizar qué porcentaje de nuestro código está siendo cubierto por nuestras pruebas, lo cual es fundamental para identificar posibles áreas que necesitan ser probadas con mayor detalle.

También es importante tener en cuenta que, en Elixir, se recomienda escribir pruebas unitarias en lugar de pruebas de integración. Esto se debe a que las pruebas unitarias son más fáciles de mantener y permiten una mayor flexibilidad en el diseño de nuestro código.

Otra buena práctica es escribir pruebas antes de escribir el código en sí, utilizando el enfoque de TDD (Test Driven Development). Esto nos ayuda a pensar en el diseño de nuestro código y en los posibles casos de uso antes de comenzar a implementarlo.

## Ver también / Enlaces útiles

- Documentación oficial de ExUnit: https://hexdocs.pm/ex_unit/ExUnit.html
- Tutorial de Elixir School sobre pruebas en Elixir: https://elixirschool.com/es/lessons/advanced/testing/
- Repositorio de ExCoveralls: https://github.com/parroty/excoveralls