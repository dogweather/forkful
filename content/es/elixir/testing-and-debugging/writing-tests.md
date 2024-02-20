---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:13.266890-07:00
description: "Escribir pruebas en Elixir implica crear scripts automatizados para\
  \ validar el comportamiento de tu c\xF3digo. Los programadores hacen esto para asegurar\
  \ la\u2026"
lastmod: 2024-02-19 22:05:17.292957
model: gpt-4-0125-preview
summary: "Escribir pruebas en Elixir implica crear scripts automatizados para validar\
  \ el comportamiento de tu c\xF3digo. Los programadores hacen esto para asegurar\
  \ la\u2026"
title: Escribiendo pruebas
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Escribir pruebas en Elixir implica crear scripts automatizados para validar el comportamiento de tu código. Los programadores hacen esto para asegurar la calidad, prevenir regresiones y facilitar la refactorización del código, haciendo el proceso de desarrollo más confiable y eficiente.

## Cómo:
Elixir utiliza ExUnit como su marco de pruebas integrado, el cual es extremadamente poderoso y fácil de usar. Aquí hay un ejemplo básico:

1. Crea un nuevo archivo de prueba en el directorio `test` de tu proyecto Elixir. Por ejemplo, si estás probando un módulo llamado `MathOperations`, tu archivo de prueba podría ser `test/math_operations_test.exs`.

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # Este es un caso de prueba simple para verificar la función de adición
  test "la adición de dos números" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

Para ejecutar tus pruebas, usa el comando `mix test` en tu terminal. Si la función `MathOperations.add/2` suma correctamente dos números, verás una salida similar a:

```
..

Finalizado en 0.03 segundos
1 prueba, 0 fallos
```

Para pruebas que involucren servicios externos o APIs, podrías querer usar bibliotecas de simulación, como `mox`, para evitar impactar servicios reales:

1. Agrega `mox` a tus dependencias en `mix.exs`:

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # otras deps...
  ]
end
```

2. Define un módulo de simulación en tu ayudante de pruebas (`test/test_helper.exs`):

```elixir
Mox.defmock(MockDelClienteHTTP, for: ComportamientoDelClienteHTTP)
```

3. Usa la simulación en tu caso de prueba:

```elixir
# test/some_api_client_test.exs
defmodule PruebaDeAlgunClienteAPI do
  use ExUnit.Case
  import Mox

  # Esto le dice a Mox que verifique si esta simulación fue llamada como se esperaba
  setup :verify_on_exit!

  test "obtiene datos de la API" do
    # Prepara la respuesta de la simulación
    expect(MockDelClienteHTTP, :get, fn _url -> {:ok, "Respuesta simulada"} end)
    
    assert AlgunClienteAPI.get_data() == "Respuesta simulada"
  end
end
```

Cuando ejecutas `mix test`, esta configuración te permite aislar tus pruebas unitarias de dependencias externas reales, enfocándote en el comportamiento de tu propio código. Este patrón asegura que tus pruebas se ejecuten rápidamente y permanezcan confiables, independientemente del estado del servicio externo o la conectividad a Internet.
