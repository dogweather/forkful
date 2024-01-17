---
title:                "Comparando dos fechas"
html_title:           "Bash: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Comparar dos fechas en la programación es una manera de determinar si una fecha es anterior, posterior o igual a otra fecha. Los programadores lo hacen para ordenar datos y realizar acciones basadas en la cronología de las fechas.

## ¿Cómo hacerlo?:
Aquí hay un ejemplo de Bash para comparar dos fechas y mostrar el resultado:

```Bash
date1="2020-10-20"
date2="2020-10-25"

if [[ "$date1" > "$date2" ]]; then
  echo "La primera fecha es posterior a la segunda."
elif [[ "$date1" < "$date2" ]]; then
  echo "La primera fecha es anterior a la segunda."
else
  echo "Ambas fechas son iguales."
fi
```
Output:
```
La primera fecha es anterior a la segunda.
```

También se pueden usar operadores lógicos como `==`, `!=`, `<=` y `>=` para comparar fechas en Bash.

## Profundizando:
En la programación, es común necesitar comparar fechas para realizar tareas como ordenar eventos cronológicos, programar tareas y generar informes basados en la fecha. Además de Bash, también hay otras herramientas y lenguajes que se pueden utilizar para comparar fechas, como Python, Java y SQL.

Bash utiliza la variable de entorno `LC_TIME` para determinar el formato de fecha y hora, lo que puede afectar la comparación de fechas en diferentes sistemas. Para evitar errores, se pueden establecer explícitamente el formato de fecha y hora deseado utilizando el comando `date -d`.

## Ver también:
- [Tutorial de Bash - Comparación de fechas](https://www.tutorialspoint.com/unix_commands/unix_comparisons.htm)
- [Documentación de Bash sobre variables de entorno](https://tldp.org/LDP/abs/html/internalvariables.html#LOCDATE)
- [Documentación de Bash sobre el comando `date`](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)