---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Bash: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Convertir una fecha en una cadena es un proceso común en la programación de Bash. Es una forma de transformar una fecha en formato de texto, lo que nos permite manipularla y utilizarla en diferentes formas en nuestros scripts. Es una herramienta útil para trabajar con datos de fecha y hora en el mundo de la programación.

## ¿Cómo hacerlo?

```Bash
$ date=$(date +%Y-%m-%d)
$ echo $date
2021-03-10
```

Este es un ejemplo básico de cómo convertir una fecha en una cadena en Bash. Primero usamos el comando `date` junto con el parámetro `%Y-%m-%d` para obtener la fecha actual en el formato deseado. Luego asignamos esa fecha a una variable `date` y la imprimimos con el comando `echo`. Esto nos dará una salida similar a la del ejemplo, pero con la fecha actual.

```Bash
$ date=$(date -d "5 days ago" +"%B %d, %Y")
$ echo $date
March 05, 2021
```

También podemos especificar una fecha anterior o posterior utilizando el parámetro `-d` junto con una cadena que indique cuántos días queremos retroceder o adelantar en el tiempo. En este ejemplo, utilizamos la fecha de hace cinco días y la imprimimos en un formato diferente utilizando el parámetro `%B` para el mes en formato completo y `%d` para el día en número.

```Bash
$ date=$(date -d "next Tuesday" +"%A %b %d, %Y")
$ echo $date
Tuesday Mar 16, 2021
```

Otra opción es especificar un día de la semana, como "next Tuesday", y el comando `date` nos dará la fecha correspondiente para ese día en particular. También podemos combinar diferentes parámetros y formatos para obtener la fecha exacta que necesitamos para nuestro script.

## Inmersión profunda

La conversión de fechas en cadenas en Bash es una técnica muy útil para trabajar con fechas y horas en nuestros scripts. Se ha vuelto cada vez más popular debido a la creciente cantidad de datos que necesitan ser procesados y manipulados de manera eficiente. Además de la opción de `date`, también hay otras herramientas y bibliotecas disponibles para realizar la conversión de manera similar, como `strftime` y `mktime`.

## También puedes ver

- Documentación de Bash: https://www.gnu.org/software/bash/
- Guía de referencia de `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html