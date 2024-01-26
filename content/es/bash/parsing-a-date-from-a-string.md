---
title:                "Análisis de una fecha a partir de una cadena"
date:                  2024-01-20T15:34:47.945021-07:00
html_title:           "Arduino: Análisis de una fecha a partir de una cadena"
simple_title:         "Análisis de una fecha a partir de una cadena"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Parsear fechas de strings permite convertir texto en datos de fecha manipulables. Los programadores lo hacen para validar, almacenar o cambiar el formato de las fechas para su uso en aplicaciones y scripts.

## ¿Cómo hacerlo?
```Bash
#!/bin/bash
fecha_str="2023-03-15 14:23:00"
fecha_formateada=$(date -d "$fecha_str" '+%d/%m/%Y')
echo $fecha_formateada
```
Salida:
```
15/03/2023
```

## Inmersión Profunda
En el pasado, los scripts de Bash confiaban en herramientas externas como `awk` o `sed` para parsear fechas. Ahora `date` es una herramienta poderosa que viene incorporada. Otras opciones podrían ser usar `dateutils` o comandos como `gawk` para mayor flexibilidad. La implementación varía según la localización y el formato de fecha deseado, usando `+%d`, `+%m`, `+%Y` para día, mes y año respectivamente.

## Ver También
- Manual de `date` de GNU: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- `strftime` para formatear fechas: http://man7.org/linux/man-pages/man3/strftime.3.html
- Bash Scripting Guide: http://www.tldp.org/LDP/abs/html/
