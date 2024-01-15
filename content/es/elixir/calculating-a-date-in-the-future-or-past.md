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

## ¿Por qué deberías calcular una fecha en el futuro o pasado?

Irás a una fiesta en dos semanas y quieres saber qué día será dentro de un mes. O tal vez estás planificando un viaje y necesitas saber qué día cae en tu cumpleaños en tres años. Sea cual sea tu razón, el cálculo de fechas en el futuro o pasado es una habilidad útil para tener en tu caja de herramientas de programación.

## Cómo hacerlo

Usando Elixir, calcular una fecha en el futuro o pasado es realmente sencillo. Simplemente tienes que usar la función ```Date.add```, que toma tres argumentos: la fecha inicial, el número de días a añadir o restar, y la unidad de tiempo (días, semanas, meses, años). Aquí hay un ejemplo de código que calcula la fecha dentro de 2 semanas:

```Elixir
initial_date = Date.today() # Esta será la fecha inicial
future_date = Date.add(initial_date, 14, :days) # 14 días en el futuro
IO.puts(future_date) # Imprime el resultado: 2020-08-05
```

Como puedes ver, simplemente añadimos 14 días a la fecha inicial y obtuvimos la fecha dentro de 2 semanas. También puedes cambiar la unidad de tiempo a semanas, meses o años según tus necesidades.

## Profundizando

Aunque la función ```Date.add``` es útil para calcular fechas en el futuro o pasado, también hay otras funciones que se pueden utilizar para hacer cálculos más complejos. Por ejemplo, la función ```Date.diff``` te permite obtener la diferencia entre dos fechas en días, semanas, meses o años. También hay funciones para obtener el primer o último día del mes y funciones para validar si una fecha es válida o no.

En resumen, el cálculo de fechas en el futuro o pasado en Elixir es muy fácil y hay varias funciones útiles que puedes utilizar para hacer cálculos más complejos. ¡Ahora tienes una nueva habilidad para sorprender a tus amigos en la próxima fiesta!

## Ver También

- Documentación de Elixir sobre fechas: https://hexdocs.pm/elixir/Date.html
- Tutorial de Elixir sobre fechas: https://elixir-lang.org/getting-started/dates-times-and-strings.html#dates-and-times-in-elixir
- Ejemplos de código para calcular fechas en Elixir: https://gist.github.com/aaronrenner/155add21f5d572ccd193c5c6bd0a4bb5