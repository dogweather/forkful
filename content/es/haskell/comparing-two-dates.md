---
title:                "Comparando dos fechas"
html_title:           "Haskell: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Comparar dos fechas es una tarea común para los programadores, ya que les permite determinar la diferencia entre dos momentos en el tiempo. Esto es especialmente útil para aplicaciones que involucran eventos, citas o plazos. Al comparar dos fechas, los programadores pueden calcular la duración exacta entre ellas y tomar decisiones basadas en eso.

## Cómo:
Veamos cómo puedes comparar dos fechas en Haskell utilizando la librería "Data.Time". Primero, debes importar la librería en tu código:

```Haskell
import Data.Time
```

Luego, puedes crear dos objetos de tipo "Day" (que representan una fecha) y utilizar la función "diffDays" para obtener la diferencia entre ellas en días.

```Haskell
let fecha1 = fromGregorian 2021 9 1 -- 1 de Septiembre de 2021
let fecha2 = fromGregorian 2021 9 15 -- 15 de Septiembre de 2021
let diferencia = diffDays fecha2 fecha1 -- devuelve un tipo Int con el valor de días entre las dos fechas
```

## Deep Dive:
Comparar dos fechas ha sido una tarea importante en la programación desde los primeros días de las computadoras. En décadas pasadas, los programadores tenían que escribir código complejo para manejar fechas y tiempos. Sin embargo, gracias a librerías como "Data.Time", esta tarea se ha vuelto mucho más simple y eficiente.

Existen otras formas de comparar fechas en Haskell, como utilizando patrones de coincidencia en una función definida por el usuario. Además, es importante tener en cuenta la diferencia entre comparar dos fechas con precisión en días, o en horas, minutos e incluso segundos. Todo depende del contexto de tu aplicación y lo que necesites calcular.

## See Also:
- Documentación para la librería "Data.Time": https://hackage.haskell.org/package/time/docs/Data-Time.html
- Ejemplos de comparación de fechas en Haskell: https://wiki.haskell.org/Date_and_time
- Una guía detallada sobre cómo manejar fechas en Haskell: https://www.fpcomplete.com/haskell/tutorial/times-and-dates/