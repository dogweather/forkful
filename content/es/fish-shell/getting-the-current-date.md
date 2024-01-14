---
title:                "Fish Shell: Obteniendo la fecha actual"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo puedes obtener la fecha actual en tu terminal? Saber la fecha puede ser útil en muchas situaciones, como por ejemplo cuando estás organizando tus archivos o cuando necesitas marcar una fecha para un recordatorio. En este artículo te explicaremos cómo obtener la fecha actual en Fish Shell y algunas de sus funciones.

## Cómo hacerlo

Para empezar, puedes utilizar el comando `date` para obtener la fecha actual en tu terminal. Por ejemplo:

```Fish Shell
date
```
```
dom dic 20 17:30:01 EST 2020
```

Sin embargo, esto nos da una fecha en un formato bastante específico que puede ser difícil de entender para algunos usuarios. Por eso, Fish Shell tiene una función llamada `strftime` que nos permite dar formato a la fecha de acuerdo a nuestras preferencias. Por ejemplo:

```Fish Shell
echo (date | strftime "%d de %B del %Y")
```
```
20 de diciembre del 2020
```

Con esta función, podemos especificar cómo queremos que se muestre la fecha utilizando diferentes símbolos. Por ejemplo, `%d` representa el día del mes, `%B` el nombre del mes y `%Y` el año completo. En la [documentación](https://fishshell.com/docs/current/cmds/date.html) de Fish Shell puedes encontrar todos los símbolos disponibles y cómo utilizarlos.

## Profundizando

Si quieres obtener más información sobre la fecha actual, Fish Shell también tiene la función `date`. Por ejemplo, puedes utilizar `date +"%s"` para obtener la fecha en formato Unix y `date +"%z"` para obtener la zona horaria en la que te encuentras.

Otra función interesante es `epoch`, que nos da la cantidad de segundos que han pasado desde el 1 de enero de 1970 hasta la fecha actual. Esta función puede ser útil para calcular la diferencia de tiempo entre dos fechas.

## Ver también

- [Documentación de Fish Shell sobre el comando `date`](https://fishshell.com/docs/current/cmds/date.html)
- [Lista de símbolos para la función `strftime`](http://www.cplusplus.com/reference/ctime/strftime/)
- [Más información sobre la función `epoch`](https://fishshell.com/docs/current/commands.html#epoch)