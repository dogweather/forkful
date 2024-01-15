---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Fish Shell: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has necesitado calcular una fecha en el futuro o en el pasado? Ya sea para programar una tarea o para planificar un proyecto, la funcionalidad de calcular fechas puede ser muy útil en tu día a día. En este artículo, te mostraremos cómo hacerlo utilizando Fish Shell.

## Cómo hacerlo

La sintaxis para calcular una fecha en el futuro o en el pasado en Fish Shell es la siguiente:

```Fish Shell
date -d "yyyy-mm-dd + X days/weeks/months/years"
date -d "yyyy-mm-dd - X days/weeks/months/years"
```

Donde "yyyy-mm-dd" corresponde a la fecha de referencia y "X" es el número de días, semanas, meses o años que quieres sumar o restar.

Por ejemplo, si queremos saber la fecha dentro de 2 semanas a partir de hoy, utilizaríamos el siguiente comando:

```Fish Shell
date -d "now + 2 weeks"
```

Y el resultado sería la fecha exacta de dentro de 2 semanas en formato "aaaa-mm-dd".

## Profundizando

Fish Shell utiliza la herramienta de línea de comandos "date" para calcular fechas. Esta herramienta es muy versátil y nos permite también hacer conversiones de fechas, mostrando el día de la semana, entre otras funciones.

Además, si utilizamos la opción "-I" en vez de "-d", podemos invertir la operación y calcular cuántos días, semanas, meses o años hay entre dos fechas dadas.

Recuerda que siempre puedes ejecutar "date --help" para obtener más información sobre cómo utilizar esta herramienta.

## Ver también

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Manual de la herramienta "date"](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)