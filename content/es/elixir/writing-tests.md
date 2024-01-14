---
title:    "Elixir: Escribiendo pruebas"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por qué

Escribir pruebas es una parte esencial del proceso de desarrollo de software. Son la forma más efectiva de garantizar que nuestro código funciona como se espera y nos ayuda a identificar y solucionar posibles errores. Además, las pruebas nos permiten llevar a cabo cambios en el código con confianza y reducir los posibles problemas en entornos de producción.

## Cómo hacerlo

Para escribir pruebas en Elixir, utilizaremos el módulo `ExUnit`, que es el framework de pruebas integrado en el lenguaje. Comencemos creando un archivo de prueba `my_module_test.exs` en la misma carpeta que nuestro módulo `my_module.ex` y agreguemos lo siguiente:

```elixir
defmodule MyModuleTest do
  use ExUnit.Case

  # Use `test` para definir una prueba
  test "sumar dos números" do
    assert 2 + 3 == 5
  end

  # Use `test` para definir una prueba
  test "multiplicar dos números" do
    assert 5 * 5 == 25
  end
end
```

En la primera línea, definimos nuestro módulo de prueba y usamos la macro `use ExUnit.Case` para importar las funcionalidades necesarias. Luego, utilizamos la macro `test` para definir una prueba. Dentro de cada prueba, usamos la función `assert` para verificar si una expresión es verdadera. Una vez que tenemos nuestras pruebas definidas, podemos ejecutarlas con el siguiente comando en la terminal:

```
mix test
```

Esto ejecutará todas las pruebas en el archivo `my_module_test.exs` y nos mostrará una salida similar a la siguiente:

```
Finished in 0.04 seconds
2 tests, 0 failures
```

¡Felicidades, acabas de escribir tus primeras pruebas en Elixir! Ahora podemos sentirnos seguros de que nuestra función `MyModule.sum/2` y `MyModule.multiply/2` funcionarán como se espera.

## Profundizando

En lugar de solo comparar el resultado de una expresión con un valor esperado, también podemos usar patrones en nuestras pruebas. Por ejemplo:

```elixir
test "obtener los primeros tres elementos" do
  assert [1, 2, 3, 4] =~ [1, 2, 3]
end
```

En este ejemplo, en lugar de usar el operador `==` para comparar el resultado con `[1, 2, 3]`, utilizamos el operador `=~` que permite el uso de patrones. Esto significa que nuestra prueba pasará siempre y cuando el resultado contenga al menos los primeros tres elementos en el orden correcto.

Otra característica útil de `ExUnit` es la posibilidad de organizar nuestras pruebas en diferentes contextos. Por ejemplo:

```elixir
defmodule MyModuleTest do
  use ExUnit.Case

  # Definimos un contexto para nuestras pruebas de sumas
  describe "sumas" do
    test "sumar dos números positivos" do
      assert 2 + 3 == 5
    end

    test "sumar un número positivo y uno negativo" do
      assert 2 + -3 == -1
    end
  end

  # Definimos un contexto para nuestras pruebas de multiplicaciones
  describe "multiplicaciones" do
    test "multiplicar dos números positivos" do
      assert 5 * 5 == 25
    end

    # Una prueba fallida para ilustrar cómo se vería
    test "multiplicar un número positivo y uno negativo" do
      assert 2 * -3 == -6
    end
  end
end
```

Al ejecutar estas pruebas, veremos una salida como esta:

```
Finished in 0.05 seconds
4 tests, 1 failure
```

Esto nos muestra claramente en qué contexto falló la prueba y en qué línea específica.

Otra funcionalidad interesante de `ExUnit` es la capacidad de ejecutar solo ciertas pruebas o contextos, lo que es útil para depurar problemas específicos o realizar pruebas en partes específicas de nuestro código. Por ejemplo, para ejecutar solo las pruebas en el contexto de sumas, podemos agregar el siguiente comando en la terminal:

```
mix test test/my_module_test.exs:20
```

Donde `20` es la