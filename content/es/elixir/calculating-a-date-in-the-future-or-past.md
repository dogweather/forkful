---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Elixir: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Calcular una fecha en el futuro o el pasado se refiere a determinar una fecha a un número específico de días, meses o años hacia adelante o hacia atrás a partir de una fecha dada. Los programadores lo hacen principalmente para funciones de manejo de tiempo, como generar fechas de vencimiento, marcar sprints de proyectos, entre otros.

## Cómo hacerlo:

¡Vamos a calcular fechas en Elixir!

```Elixir
defmodule Prueba do
  alias Date, as: D
  def futuro(fecha, dias) do
    D.add(fecha, dias)
  end

def pasado(fecha, dias) do
  D.add(fecha, -dias)
end
end

IO.inspect(Prueba.futuro(D.today(), 10))
IO.inspect(Prueba.pasado(D.today(), 10))
```

Aquí tenemos una muestra de salida:

```Elixir
~D[2022-09-10]  # 10 días en el futuro desde el 31 de agosto
~D[2022-08-21]  # 10 días en el pasado desde el 31 de agosto
```

## Análisis profundo:

Históricamente, los lenguajes de programación han tenido diferentes formas de manejar fechas y tiempos, y Elixir no es una excepción. La función `Date.add/2` se introdujo en la versión 1.3 y proporciona una forma concisa de agregar o restar días a una fecha.

Existen alternativas para hacer este cálculo, como usar la librería Timex, que es una poderosa herramienta para manipular fechas, pero para propósitos simples como el nuestro, el módulo Date interno está más que suficiente.

En cuanto a detalles de implementación, `Date.add/2` simplemente crea una nueva fecha a partir de la fecha y la cantidad de días proporcionados. Si el número es negativo, da como resultado una fecha en el pasado.

## Ver también:

Aquí hay algunos recursos adicionales sobre el manejo de fechas en Elixir:
1. [Documentación oficial de Elixir para `Date`](https://hexdocs.pm/elixir/Date.html)
2. [`Date.add/2` en HexDocs](https://hexdocs.pm/elixir/Date.html#add/2)
3. [Timex en GitHub](https://github.com/bitwalker/timex)