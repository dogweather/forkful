---
title:                "Fish Shell: Comparando dos fechas"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué
Comparar dos fechas en programación puede ser muy útil para determinar la duración de un evento, calcular el tiempo transcurrido entre dos puntos o simplemente para ordenar eventos cronológicamente. En este artículo, aprenderemos cómo comparar fechas usando Fish Shell.

## Cómo hacerlo
La comparación de fechas en Fish Shell se realiza con el comando `date`. Para comparar dos fechas, debemos asegurarnos de que ambas estén en el formato adecuado, es decir, en el orden de año, mes y día. Por ejemplo, si queremos comparar las fechas 01/01/2020 y 02/01/2020, deberíamos escribirlo como 20200101 y 20200102 respectivamente.

Para comparar estas dos fechas, podemos usar el comando `test` junto con la opción `-lt` (menor que) o `-gt` (mayor que), seguido de las dos fechas en el formato mencionado anteriormente. Veamos un ejemplo:

```Fish Shell
if test 20200101 -lt 20200102
    echo "La primera fecha es anterior a la segunda."
end
```

En el código anterior, usamos el comando `test` para comparar las dos fechas y luego imprimimos un mensaje en caso de que la primera fecha sea anterior a la segunda. Si cambiamos la opción a `-gt`, el mensaje se imprimirá solo si la primera fecha es mayor que la segunda.

## Profundizando
Hay algunas cosas más que debemos tener en cuenta al comparar fechas en Fish Shell. Por ejemplo, si queremos comparar fechas con horas incluidas, debemos escribir la fecha en el siguiente formato: `AAAAMMDDTHHMMSS`. Además, el comando `test` también tiene opciones como `-ge` (mayor o igual que) o `-le` (menor o igual que) que nos permiten realizar comparaciones más complejas.

Otra cosa importante a tener en cuenta es el uso de variables. En lugar de escribir las fechas directamente en el comando `test`, podemos guardarlas en variables y luego utilizar esas variables en la comparación. Esto hace que nuestro código sea más legible y fácil de modificar si es necesario.

## Ver también
- [Documentación de Fish Shell sobre el comando `date`](https://fishshell.com/docs/current/cmds/date.html)
- [Más información sobre el comando `test`](https://fishshell.com/docs/current/cmds/test.html)
- [Ejemplos de comparación de fechas en Fish Shell](https://fishshell.com/docs/current/tutorial.html#test-command)