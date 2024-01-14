---
title:    "Elixir: Escribiendo pruebas"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué escribir pruebas en Elixir

Escribir pruebas en Elixir puede parecer una tarea tediosa y adicional al momento de programar. Sin embargo, es una práctica esencial ya que asegura que nuestro código funcione correctamente y nos ayuda a encontrar errores antes de que puedan afectar a nuestros usuarios. Además, escribir pruebas también nos permite tener un mejor entendimiento del código que estamos creando.

## Cómo escribir pruebas en Elixir

Para escribir pruebas en Elixir, utilizamos el módulo `ExUnit` que viene incluido en la librería estándar de Elixir. Primero, debemos importar este módulo en nuestro archivo de pruebas. Luego, podemos definir nuestras pruebas utilizando la macro `test` y brindándole un nombre descriptivo y una función anónima que contenga el código que queremos probar.

```Elixir
defmodule Pruebas do
  use ExUnit.Case

  test "suma de numeros" do
    assert 2 + 2 == 4
  end
end
```
Una vez que hemos definido nuestras pruebas, podemos ejecutarlas utilizando el comando `mix test` en nuestra terminal. Si todas las pruebas pasan exitosamente, recibiremos un mensaje de `1 test, 0 failures`.

## Profundizando en la escritura de pruebas

Escribir pruebas nos ayuda a identificar rápidamente errores en nuestro código y nos permite tener una mayor confianza en su funcionamiento. Además, nos permite tener una documentación viva de nuestro código, ya que las pruebas actúan como ejemplos de cómo utilizar nuestras funciones. Para obtener más información sobre cómo escribir pruebas en Elixir, puedes revisar la documentación oficial de `ExUnit`.

## Ver también

- [Documentación oficial de ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Artículo: "Guía de pruebas en Elixir"](https://happybearsoftware.com/guides/elixir/testing-in-elixir/)
- [Video: "Testing Your Phoenix App"](https://www.youtube.com/watch?v=zy9JDWJ-ZVQ)