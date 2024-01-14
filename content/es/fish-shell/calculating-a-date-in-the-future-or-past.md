---
title:    "Fish Shell: Calculando una fecha en el futuro o pasado."
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué 

A veces, necesitamos predecir una fecha futura o calcular una fecha en el pasado. Puede ser para planificar eventos, programar tareas o simplemente por curiosidad. ¡Afortunadamente, Fish Shell hace que esto sea fácil con su funcionalidad de cálculo de fecha! Sigue leyendo para aprender cómo hacerlo.

## Cómo hacerlo

Para calcular una fecha en el futuro, usaremos el comando `date` seguido de un número y la unidad de tiempo deseada, como días, meses o años. Por ejemplo, para obtener la fecha de mañana, escribiremos:

```Fish Shell
date 1d
```

Esto nos dará la fecha de mañana en el formato de día de la semana, mes y día. Si queremos una fecha específica, podemos usar el formato de "año-mes-día". Por ejemplo:

```Fish Shell
date 2021-08-30
```

Esto nos dará la fecha del 30 de agosto de 2021. También podemos sumar o restar unidades de tiempo a fechas existentes. Por ejemplo:

```Fish Shell
date 2021-08-30 - 2w
```

Esto nos dará la fecha 2 semanas antes del 30 de agosto de 2021. ¡Genial!

## Profundizando

Fish Shell utiliza el formato de fecha "año-mes-día" para realizar cálculos. Si necesitas ayuda con el formato, siempre puedes consultar la página de manual de `date` escribiendo `man date` en tu terminal.

Además de calcular fechas en el futuro, Fish Shell también puede calcular fechas en el pasado. Para hacerlo, solo necesitamos escribir un número negativo antes de la unidad de tiempo deseada. Por ejemplo:

```Fish Shell
date -1y
```

Esto nos dará la fecha de hace un año en el formato de "año-mes-día". También podemos realizar cálculos más complejos, como sumar o restar múltiples unidades de tiempo a la vez. Por ejemplo:

```Fish Shell
date 2021-08-30 + 2y - 1m + 3w - 5d
```

Esto nos dará la fecha 2 años después, 1 mes antes, 3 semanas después y 5 días antes del 30 de agosto de 2021.

## Ver también

- [Página de manual de `date`](https://fishshell.com/docs/current/cmds/date.html)
- [Preguntas frecuentes sobre fechas en Fish Shell](https://github.com/fish-shell/fish-shell/wiki/FAQ#dates-and-time)