---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:12:57.985215-07:00
simple_title:         "Obteniendo la fecha actual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué & Por qué?
Obtener la fecha actual en Bash es leer el momento presente del sistema. Los programadores lo usan para registros, scripts cronometrados y funciones que dependen del tiempo.

## Cómo hacerlo:
Sencillo y directo. Usa `date` para obtener la información que necesitas.

```Bash
# Simple uso para obtener la fecha y hora actual
echo $(date)

# Formato personalizado para fecha: YYYY-MM-DD
echo $(date '+%Y-%m-%d')

# Guardar fecha en una variable
current_date=$(date)
echo $current_date
```

Ejemplo de salida:

```Bash
Mon Mar 22 14:21:42 PDT 2023
2023-03-22
Mon Mar 22 14:21:42 PDT 2023
```

## Inmersión Profunda:
`date` es parte del GNU coreutils y existe desde las primeras versiones de Unix, lo que significa que es bastante estándar en sistemas Unix-like. Existen comandos alternativos como `hwclock` para hardware clocks y utilidades de terceros, pero `date` suele ser suficiente para la mayoría de las necesidades. La belleza está en la flexibilidad: `date` permite formatos personalizados con ‘+’ seguido de directivas de formato, tales como `%Y` para el año, `%m` para el mes y `%d` para el día.

## Ver También:
- Para más información sobre formateo de fecha en Bash, chequea el manual con `man date` en tu terminal.
- Si necesitas ejemplos más complejos y explicaciones, GNU tiene buenos recursos: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- ¿Buscas trabajar con fechas en scripts? Este tutorial es útil: https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/
