---
title:                "Manejo de errores"
aliases:
- /es/elixir/handling-errors.md
date:                  2024-01-26T00:50:41.256863-07:00
model:                 gpt-4-1106-preview
simple_title:         "Manejo de errores"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/handling-errors.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Manejar errores significa escribir código que pueda lidiar con las cosas cuando salen mal. Los programadores lo hacen para prevenir fallos y para asegurarse de que sus programas puedan recuperarse de manera controlada cuando la Ley de Murphy ataca.

## Cómo hacerlo:

En Elixir, a menudo usamos el emparejamiento de patrones y la declaración `case` para manejar diferentes resultados, incluidos los errores.

```elixir
defmodule Example do
  def divide(a, b) do
    case b do
      0 -> {:error, "No se puede dividir entre cero."}
      _ -> {:ok, a / b}
    end
  end
end

# División exitosa
{:ok, result} = Example.divide(10, 2)
IO.puts("10 / 2 es #{result}")

# Intento de dividir entre cero
{:error, reason} = Example.divide(10, 0)
IO.puts("Error: #{reason}")
```

Salida de muestra:
```
10 / 2 es 5.0
Error: No se puede dividir entre cero.
```

Cuando ejecutas este código de Elixir, obtendrás o una división exitosa o un mensaje de error, dependiendo de tu entrada. ¡Aquí no hay fallos!

## Inmersión Profunda

Hace tiempo, el manejo de errores a menudo se trataba de verificar valores de retorno. Sin embargo, con las raíces funcionales de Elixir, tenemos el emparejamiento de patrones y tuplas etiquetadas, como `{:ok, value}` o `{:error, reason}`, que son más elegantes.

Hay otras maneras de manejar errores en Elixir:

- **`try` y `rescue` de Elixir**, que se asemejan al tradicional `try-catch` en lenguajes imperativos pero se utilizan menos frecuentemente debido a la preferencia de Elixir por la explicitud.
- **Supervisores y GenServers**, parte del marco de trabajo OTP de Elixir, que se trata más de tolerancia a fallos. Observan el proceso del código, listos para reiniciarlo si algo va mal.

En términos de implementación, Elixir se basa en la robustez de Erlang. Trata los errores como otro tipo de mensaje a ser manejado con toda la bondad del emparejamiento de patrones y la funcionalidad.

## Véase también

Para lectura adicional sobre manejo de errores en Elixir, consulta:

- La guía oficial de Elixir sobre [manejo de errores](https://elixir-lang.org/getting-started/try-catch-and-rescue.html).
- Aprende más sobre [procesos y OTP](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html).
- El Foro de Elixir siempre es un buen lugar para hacer preguntas: [https://elixirforum.com](https://elixirforum.com).
