---
title:                "Analizando una fecha a partir de una cadena de texto"
html_title:           "Bash: Analizando una fecha a partir de una cadena de texto"
simple_title:         "Analizando una fecha a partir de una cadena de texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

El análisis de una fecha desde una cadena de texto es la transformación de una fecha expresada en formato texto a un formato utilizable en código. Los programadores lo hacen para manejar y realizar operaciones con fechas en sus aplicaciones.

## Cómo hacerlo:

```Fish Shell 
# Definir una cadena de fecha
set fecha "2022-02-02"

# Usar 'date' para transformar la cadena en una variable de fecha
set fecha_decodificada (date -d"$fecha")

# Imprimir la fecha decodificada
echo $fecha_decodificada
```

La ejecución de este código debutaría un argumento similar a esto:
```Fish Shell
miércoles, 02 de febrero de 2022 0:00:00 CET
```

## Análisis más profundo

Históricamente, la necesidad de analizar fechas de las cadenas de texto surge con la inclusión de información temporal en los datos de las aplicaciones. Alternativamente a 'date', Fish Shell ofrece funciones como 'strptime'de strftime disponible desde la versión 3 de Fish Shell, que permite un mayor control sobre el formato de la fecha. El análisis de fechas involucra la detección del formato de la fecha en la cadena original y su conversión en un tipo de dato de fecha del lenguaje.

## Ver también

Para más información, visita estas fuentes:


- Manual de [Fish Shell](https://fishshell.com/docs/current/index.html)