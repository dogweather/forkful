---
title:                "Trabajando con números complejos"
date:                  2024-01-26T04:38:41.595924-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con números complejos"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Los números complejos tienen una parte real y una parte imaginaria (como `3 + 4i`). Se utilizan en ingeniería, física y ciertos problemas de computación. Los programadores trabajan con ellos para simulaciones, procesamiento de señales y resolución eficiente de ciertos tipos de problemas matemáticos.

## Cómo hacerlo:
Elixir no tiene números complejos incorporados, así que creamos los nuestros o usamos una librería, como `ComplexNum`. Aquí hay un ejemplo rápido con una librería:

```elixir
# Asumiendo que tienes ComplexNum instalado
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# Crear números complejos y sumarlos
c1 = {3, 4}   # representa 3 + 4i
c2 = {2, -3}  # representa 2 - 3i
resultado = ComplexMath.add(c1, c2)
IO.puts "El resultado es: #{inspect(resultado)}"
```

Esto produciría:
```
El resultado es: {5, 1}
```

Significa que la suma de `3 + 4i` y `2 - 3i` es `5 + 1i`.

## Profundización
Los números complejos aparecieron en la historia porque los simples números de toda la vida no podían manejar las raíces cuadradas de números negativos. No fue hasta el siglo 17 que se los tomó en serio, gracias a matemáticos como René Descartes y Gerolamo Cardano.

En Elixir, a menudo se usan tuplas como `{3, 4}` para los números complejos, o se usa una librería dedicada para evitar reinventar la rueda. Las librerías son usualmente mejores: manejan los detalles complicados como la multiplicación y división, que se vuelven complicados debido a la unidad imaginaria 'i' (Para tu información: `i` al cuadrado es igual a `-1`).

## Ver También
Consulta estos recursos:
- [Librería ComplexNum](https://hex.pm/packages/complex_num) para el gestor de paquetes de Elixir, Hex.
- [Escuela de Elixir](https://elixirschool.com/en/), para temas avanzados de Elixir y ejercicios.
- [Erlang -- módulo math](http://erlang.org/doc/man/math.html), que Elixir usa internamente, para otras necesidades matemáticas.
