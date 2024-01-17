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

¡Hola a todos! ¡Bienvenidos a mi artículo sobre el lenguaje de programación Fish Shell! En este artículo, les mostraré cómo calcular una fecha en el futuro o en el pasado utilizando Fish Shell de forma rápida y sencilla. ¿Listos? ¡Comencemos!

## ¿Qué y por qué? 
Calcular una fecha en el futuro o en el pasado es una tarea común para los programadores. Esto se debe a que, en muchos casos, necesitamos manipular fechas en nuestras aplicaciones para realizar cálculos o mostrar información en un formato específico. Fish Shell nos ofrece una manera fácil y eficiente de hacerlo con su funcionalidad de fechas integrada.

## Cómo hacerlo:
Para calcular una fecha en el futuro o en el pasado, utilizamos el comando `date` seguido de un operador (+ o -) y un número que represente la cantidad de tiempo que deseamos agregar o restar a la fecha actual. Por ejemplo, si queremos calcular la fecha dentro de una semana, escribimos `date +1w`.

### Ejemplos de código:
```
# Calculando la fecha dentro de una semana
Fish Shell> date +1w
Wed Dec 23 23:51:29 EST 2020

# Calculando la fecha hace 2 meses
Fish Shell> date -2m
Fri Oct 23 23:52:08 EST 2020
```

## Profundizando:
Fish Shell utiliza el estándar [GNU Core Utilities date](https://www.gnu.org/software/coreutils/manual/html_node/Calendar-date-items.html) para manejar fechas y horas. Además, existen otras alternativas como `chrony` y `ntpdate` que también pueden ser útiles para realizar cálculos de fechas.

## Ver también:
Si quieren saber más sobre la funcionalidad de fechas en Fish Shell, pueden consultar la [documentación oficial](https://fishshell.com/docs/current/index.html#time). También pueden encontrar más información sobre el comando `date` en la [página de manual](https://fishshell.com/docs/current/cmds/date.html) de Fish Shell.

¡Y eso es todo por hoy! Espero que este artículo les haya sido útil y que ahora puedan calcular fechas en el futuro o en el pasado sin problemas utilizando Fish Shell. ¡Hasta la próxima!