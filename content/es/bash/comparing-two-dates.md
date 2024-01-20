---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Comparación de Fechas en Bash: Un enfoque simple

## ¿Qué y Por qué?
La comparación de dos fechas es un proceso que determina qué fecha es más reciente o si ambas son iguales. Los programadores lo utilizan para programar tareas, organizar eventos cronológicamente y monitorear plazos.

## ¿Cómo hacerlo?
Aquí te dejo un script básico en Bash para que veas cómo se compara dos fechas. El script obtiene dos fechas como argumentos y muestra cuál de ellas es más reciente.

```Bash
#!/bin/bash

fecha1="$1"
fecha2="$2"

dato1=$(date -d"$fecha1" +%s)
dato2=$(date -d"$fecha2" +%s)

if [ $dato1 -eq $dato2 ]
then
  echo "Las fechas son iguales."
elif [ $dato1 -gt $dato2 ]
then
  echo "$fecha1 es más reciente que $fecha2."
else
  echo "$fecha2 es más reciente que $fecha1."
fi
```
Sólo tienes que ejecutar el script de esta forma, pasando dos fechas como argumentos en el termina:

```Bash
bash comparar_fechas.sh "2022-09-05" "2022-09-04"
```

Esto te dará la salida:

```Bash
2022-09-05 es más reciente que 2022-09-04.
```

## Más Detalles
La comparación de fechas ha sido una función deseada durante mucho tiempo en programación Bash, pero solo fue posible después de la versión Bash 4.0 en 2009 cuando se introdujo el comando `date` con la opción -d. Aunque hay otros lenguajes de programación que pueden realizar esta tarea con mayor eficacia, el Bash sigue siendo una opción atractiva para los usuarios de Linux y MacOS debido a su simplicidad y accesibilidad directa en el terminal. 

Otra alternativa para comparar fechas es usar `awk` o `perl`, pero eso agregaría más complejidad a la tarea. En lugar de eso, `date -d` convierte la fecha en segundos desde la época UNIX (1970-01-01 00:00:00 UTC) para facilitar la comparación.

## Ver También
Para obtener más detalles, consulta la guía GNU sobre el comando `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation

Si tienes curiosidad acerca de los detalles de la fecha UNIX, consulta: https://es.wikipedia.org/wiki/Tiempo_Unix