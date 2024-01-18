---
title:                "Analizando una fecha de una cadena"
html_title:           "Bash: Analizando una fecha de una cadena"
simple_title:         "Analizando una fecha de una cadena"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Parsing de fecha es un término técnico que se refiere a tomar una fecha escrita en formato de texto y convertirla en un formato de fecha reconocible por una computadora. Los programadores hacen esto para poder manipular y utilizar fechas en sus códigos de manera efectiva.

## Cómo hacerlo:

El parsing de fecha en Bash se puede realizar utilizando la herramienta `date` y su argumento `--date`. Por ejemplo, para convertir una fecha en formato de texto a un formato de fecha específico, puedes usar el siguiente comando:

```Bash
date --date="2021-07-15" +"%m/%d/%Y"
```
Esto producirá un output de `07/15/2021`, indicando que la fecha ha sido parseada exitosamente.

También puedes utilizar la opción `-d` para indicar que la fecha a ser parseada está en un formato específico. Por ejemplo:

```Bash
date -d "20 julio 2021" +"%Y/%m/%d"
```
Esto generará un resultado de `2021/07/20` ya que se ha especificado que la fecha está escrita en formato DD MMMM YYYY.

## Profundizando:

El parsing de fecha ha sido una tarea importante para programadores desde los inicios de la informática, ya que permite trabajar con fechas y manejarlas de manera efectiva. Dependiendo del lenguaje de programación, puede haber diferentes métodos y funciones para realizar el parsing de fecha.

Otra herramienta popular en Bash para realizar operaciones de fecha y tiempo es `awk`, la cual también puede ser utilizada para parsear fechas. Sin embargo, la herramienta `date` es más específica y fácil de utilizar para este propósito.

Además, el parsing de fecha puede ser de gran utilidad en tareas como generar informes financieros, realizar análisis de datos, o automatizar procesos basados en fechas.

## Ver también:

Si deseas aprender más sobre parsing de fecha en Bash, puedes consultar la documentación oficial de `date` y `awk`.

- https://www.gnu.org/software/gawk/manual/html_node/Time-Functions.html#Time-Functions
- https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation