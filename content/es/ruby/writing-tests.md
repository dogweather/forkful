---
title:                "Escribiendo pruebas"
date:                  2024-01-19
simple_title:         "Escribiendo pruebas"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir pruebas es crear código que verifica que otro código funciona como se espera. Los programadores lo hacen para asegurar calidad, facilitar el mantenimiento y permitir el desarrollo ágil.

## Cómo Hacerlo:
Ruby usa Minitest y RSpec como sus bibliotecas de pruebas más comunes. Aquí hay un ejemplo simple con Minitest.

```Ruby
# archivo test_example.rb
require 'minitest/autorun'

class Calculadora
  def sumar(a, b)
    a + b
  end
end

class CalculadoraTest < Minitest::Test
  def test_sumar
    calc = Calculadora.new
    resultado = calc.sumar(2, 3)
    assert_equal 5, resultado
  end
end
```

Ejecuta este script en la línea de comandos y deberías ver algo como esto:

```
Run options: --seed 33443

# Running:

.

Finished in 0.001025s, 975.6098 runs/s, 975.6098 assertions/s.

1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

## Profundización
Minitest ha estado en Ruby desde su versión 1.9, reemplazando a Test::Unit. RSpec ofrece más sintaxis de "comportamiento" y es ampliamente usado en desarrollo BDD (Desarrollo Guiado por Comportamiento). Estas bibliotecas permiten TDD (Desarrollo Guiado por Pruebas), donde primero se escribe una prueba fallida y luego el código necesario para pasarla.

## Ver También
- [RSpec Website](https://rspec.info/)
- [Better Specs {Testing Guidelines}](http://www.betterspecs.org/)
- ["Why's Poignant Guide to Ruby" {For a fun introduction to Ruby}](https://poignant.guide/)
