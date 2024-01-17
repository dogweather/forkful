---
title:                "Calculando una fecha en el futuro o en el pasado."
html_title:           "Haskell: Calculando una fecha en el futuro o en el pasado."
simple_title:         "Calculando una fecha en el futuro o en el pasado."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# ¿Qué & Por qué?

Calcular una fecha en el futuro o en el pasado es un proceso común en la programación, ya que permite a los programadores trabajar con fechas y horarios de manera más eficiente. Esto se logra mediante el uso de funciones que manipulan y calculan fechas de acuerdo a ciertas reglas y algoritmos.

Los programadores realizan estos cálculos para una variedad de propósitos, como programar eventos futuros, mostrar fechas en formatos específicos, o realizar cálculos de tiempo transcurrido.

# Cómo hacerlo:

Calculando una fecha en el futuro o en el pasado es fácil en Haskell. Primero, debemos importar el módulo `Data.Time`, que proporciona funciones para manejar fechas y horarios. Luego, podemos usar la función `addDays` para agregar o restar días a una fecha dada, y la función `addUTCTime` para agregar o restar segundos a una marca de tiempo dada. Aquí hay un ejemplo de cómo usar estas funciones:

```Haskell
import Data.Time

-- Calculando una fecha en el futuro
let fechaFutura = addDays 10 (fromGregorian 2020 1 1)

-- Calculando una fecha en el pasado
let fechaPasada = addDays (-10) (fromGregorian 2020 1 1)

-- Calculando una marca de tiempo en el futuro
let marcaDeTiempoFutura = addUTCTime 60 (getCurrentTime)

-- Calculando una marca de tiempo en el pasado
let marcaDeTiempoPasada = addUTCTime (-60) (getCurrentTime)
```

# Inmersión Profunda:

En la programación, el cálculo de fechas se ha vuelto cada vez más importante debido a la creciente necesidad de trabajar con datos temporales y sincronizar aplicaciones con diferentes zonas horarias. Sin embargo, antes del advenimiento de los lenguajes de programación modernos, los cálculos de fecha eran más complicados y propensos a errores.

Además de las funciones mencionadas anteriormente, existen otras formas de calcular fechas en Haskell, como la librería `time-lens`, que proporciona un conjunto de tipos de datos y funciones para trabajar con fechas y horarios. Alternativamente, también se pueden usar librerías externas como `date` o `time`, dependiendo de las necesidades del proyecto.

En términos de implementación, Haskell cuenta con un sistema de tipos seguro y sólido que ayuda a prevenir errores de cálculo de fechas, lo que lo convierte en una elección popular para proyectos que requieren un manejo preciso de la fecha y el tiempo.

# Ver también:

- [Documentación de `Data.Time`](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Librería `time-lens`](https://hackage.haskell.org/package/time-lens)
- [Librería `date`](https://hackage.haskell.org/package/date)
- [Librería `time`](https://hackage.haskell.org/package/time)