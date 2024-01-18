---
title:                "Analizando una fecha de una cadena de texto"
html_title:           "Fish Shell: Analizando una fecha de una cadena de texto"
simple_title:         "Analizando una fecha de una cadena de texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Parsing una fecha de una cadena de texto es el proceso de extraer una fecha específica de una cadena de texto más grande. Los programadores a menudo realizan esta tarea para poder trabajar con esa fecha en su código de manera más eficiente.

## ¿Cómo?

### Código de ejemplo:

```Fish Shell
string="Hoy es 14 de octubre, 2021"
set -l date (date -f "%d de %B, %Y" $string)
echo $date 
```

### Salida del ejemplo:

```
14 de octubre, 2021
```

## Profundizando

Parsing de fechas de cadenas de texto ha sido una tarea común en programación por mucho tiempo. Antes de las herramientas y funciones dedicadas para esta tarea, los programadores tenían que escribir sus propios algoritmos para extraer fechas de cadenas de texto. Sin embargo, hoy en día, hay muchas alternativas disponibles para los programadores como la función `date` en Fish Shell. Esta función utiliza un formato especificado por el usuario para analizar una cadena de texto y extraer una fecha. Detalles sobre el formato de fecha se pueden encontrar en la documentación de Fish Shell.

## Ver también

- [La documentación oficial de `date` en Fish Shell](https://fishshell.com/docs/current/cmds/date.html)
- [Una comparación entre las diferentes formas de parsear fechas en Shell](https://opensource.com/article/17/7/how-parse-string-date-shell)
- [Un artículo sobre parseo de fechas en Bash](https://www.gnu.org/software/bash/manual/html_node/Date-and-Time.html)