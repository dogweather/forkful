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

## ¿Qué y por qué?

Calcular una fecha en el futuro o en el pasado es una tarea común en la programación, ya sea para programar eventos, notificaciones o simplemente para realizar cálculos en una aplicación. Los programadores necesitan esta funcionalidad para hacer sus aplicaciones más dinámicas y útiles.

## Cómo hacerlo:

El lenguaje de programación Elixir tiene una función útil llamada Date.add/2 que permite agregar o restar una cantidad específica de días, meses o años a una fecha dada. Aquí hay un ejemplo de cómo usarlo:

```Elixir
Date.add(date, days)
```

donde `date` es la fecha de inicio y `days` es el número de días que desea agregar o restar. Por ejemplo, si queremos obtener la fecha de 5 días después de la fecha actual, podemos llamar a la función de esta manera:

```Elixir
Date.add(Date.utc_today(), 5)
```

Esto nos dará la fecha en formato `{year, month, day}`. También podemos utilizar los módulos `DateTime` y `Timex` para obtener resultados más detallados y formateados.

## Profundizando:

Calcular fechas en el futuro o en el pasado ha sido una necesidad constante en la programación. Antes de la invención de las computadoras, las fechas y el tiempo se calculaban manualmente. Con el avance de la tecnología, se han desarrollado diferentes algoritmos y funciones para hacer este proceso más eficiente y preciso. Además de la función `Date.add/2`, también se pueden utilizar otras bibliotecas como `Chronic` y `Calendar` para realizar cálculos de fechas.

## Ver también:

- Documentación oficial de Elixir sobre Date: https://hexdocs.pm/elixir/Date.html#add/2
- Ejemplos de uso de funciones de fechas en Elixir: https://blog.appsignal.com/2019/04/23/how-to-work-with-dates-in-elixir.html
- Alternativas a la función `Date.add/2`: https://stackoverflow.com/questions/54711915/how-do-i-add-days-to-date-in-elixir