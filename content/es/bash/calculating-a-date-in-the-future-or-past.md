---
title:    "Bash: Calculando una fecha en el futuro o en el pasado."
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Muchas veces en la programación, necesitamos calcular una fecha en el futuro o en el pasado. Ya sea para programar una tarea o para hacer cálculos en una base de datos, saber cómo hacer esto es una habilidad valiosa para cualquier programador.

## Cómo hacerlo

En Bash, podemos utilizar la herramienta "date" para calcular una fecha en el futuro o en el pasado. La sintaxis básica de este comando es la siguiente:
```
date -d "fecha_baseoperaciondescripciónvalor"
```

Por ejemplo, si queremos saber la fecha que será dentro de 5 días, podemos utilizar el siguiente comando:
```
date -d "now + 5 days"
```

Esta sintaxis también puede ser utilizada para calcular fechas en el pasado, simplemente cambiando el signo "+" por "-". A continuación, se muestran algunos ejemplos más:

Calcular la fecha de 1 mes atrás desde la fecha actual:
```
date -d "now - 1 month"
```

Calcular la fecha de 2 semanas en el futuro desde una fecha específica:
```
date -d "2021-10-01 + 2 weeks"
```

Calcular la fecha actual a las 10 AM:
```
date -d "today 10 am"
```

Además, podemos utilizar valores como "seconds", "minutes", "hours" o "years" para calcular fechas en intervalos más pequeños o más grandes. Por ejemplo:
```
date -d "now + 2 hours"
```

## Profundizando

La herramienta "date" tiene muchas más opciones que nos permiten personalizar aún más nuestros cálculos de fechas. Por ejemplo, podemos utilizar la opción "-u" para mostrar la fecha en formato UTC o la opción "-f" para utilizar una fecha específica en lugar de la actual.

También podemos utilizar la opción "-r" para mostrar la diferencia entre dos fechas en lugar de mostrar una fecha específica. Por ejemplo, si queremos saber cuántos días han pasado desde el 1 de enero de 2021 hasta hoy, podemos utilizar el siguiente comando:
```
date -d "$(date -d "2021-01-01") - today"
```

Además, el comando "date" también nos permite convertir una fecha en otro formato utilizando la opción "-I". Por ejemplo, si queremos convertir una fecha del formato "dd-mm-aaaa" al formato "aaaa-mm-dd", podemos utilizar el siguiente comando:
```
date -d "4-10-2021" -I"new"
```

## Ver también 
- [Documentación de GNU Coreutils para el comando date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Tutorial de Bash para principiantes](https://linuxize.com/post/bash-scripting-tutorial-for-beginners/)