---
title:    "Fish Shell: Calculando una fecha en el futuro o pasado"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo puedes calcular la fecha de hoy en el futuro o en el pasado? Este artículo te mostrará cómo hacerlo con Fish Shell.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado, podemos utilizar el comando ``date`` de Fish Shell. Podemos especificar la cantidad de días, meses o años que queremos agregar o restar a la fecha actual.

Por ejemplo, si queremos saber la fecha dentro de 5 días, podemos utilizar el siguiente comando:

```Fish Shell
date -d "+5 days"
```

Esto nos devolverá la fecha en el formato "año-mes-día". En este caso, si hoy es 17 de mayo de 2021, el resultado sería "2021-05-22".

También podemos especificar una fecha específica como punto de partida. Por ejemplo, si queremos saber la fecha dentro de 3 meses a partir del 20 de abril de 2022, podemos utilizar el siguiente comando:

```Fish Shell
date -d "2022-04-20 +3 months"
```

El resultado sería "2022-07-20".

Además de los días y meses, también podemos agregar o restar años a la fecha actual. Por ejemplo, si queremos saber la fecha de hoy en 5 años, podemos utilizar el siguiente comando:

```Fish Shell
date -d "+5 years"
```

## Profundizando

El comando ``date`` de Fish Shell utiliza la biblioteca de GNU coreutils para realizar los cálculos de fecha. Por lo tanto, podemos referirnos a la documentación de coreutils para obtener más información sobre cómo utilizar este comando y sus opciones.

También podemos realizar cálculos más complejos, como especificar una fecha y hora exactas, utilizando el formato de fecha ISO 8601 y la opción ``-d`` para especificar una fecha. Por ejemplo, si queremos saber la fecha y hora dentro de 2 horas a partir del 6 de junio de 2021 a las 9:30 am, podemos utilizar el siguiente comando:

```Fish Shell
date -d "2021-06-06T09:30:00 +2 hours"
```

El resultado sería "2021-06-06T11:30:00".

## Ver también

- [Documentación de coreutils](https://www.gnu.org/software/coreutils/manual/html_node/)
- [Guía de referencia de Fish Shell](https://fishshell.com/docs/current/)
- [Guía de formato de fecha ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html)