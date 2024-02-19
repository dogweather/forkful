---
aliases:
- /es/fish-shell/converting-a-date-into-a-string/
date: 2024-01-20 17:36:37.416425-07:00
description: "Convertir fechas en cadenas de texto te permite formatear y mostrar\
  \ fechas de manera legible y personalizada. Los programadores lo hacen para manipular\
  \ y\u2026"
lastmod: 2024-02-18 23:09:10.465750
model: gpt-4-1106-preview
summary: "Convertir fechas en cadenas de texto te permite formatear y mostrar fechas\
  \ de manera legible y personalizada. Los programadores lo hacen para manipular y\u2026"
title: Convirtiendo una fecha en una cadena de texto
---

{{< edit_this_page >}}

## Qué y Por Qué?

Convertir fechas en cadenas de texto te permite formatear y mostrar fechas de manera legible y personalizada. Los programadores lo hacen para manipular y registrar fechas en logs, interfaces de usuario o para almacenarlas eficientemente.

## Cómo Hacerlo:

En Fish, la conversión de fechas es directa. Usa `date` para mostrar fechas en diferentes formatos:

```Fish Shell
# Formato estándar: dd/mm/yyyy
set fecha_formato_estandar (date "+%d/%m/%Y")
echo $fecha_formato_estandar

# Formato ISO 8601: yyyy-mm-dd
set fecha_iso (date "+%Y-%m-%d")
echo $fecha_iso

# Fecha y hora completa
set fecha_hora (date "+%Y-%m-%d %H:%M:%S")
echo $fecha_hora
```

Ejemplo de salida:

```
25/03/2023
2023-03-25
2023-03-25 16:45:12
```

## Deep Dive:

En Unix, el comando `date` viene de los primeros días del sistema operativo, proveiendo una manera simple de obtener la fecha y hora del sistema. 

Alternativas incluyen usar `strftime` para formatear fechas, una función disponible en varios lenguajes de programación. En Fish, la función `date` es frecuentemente suficiente y hereda mucha de su funcionalidad de las operaciones de bajo nivel del sistema operativo, lo que la hace bastante eficiente.

Un detalle interesante es que la representación interna de las fechas en las computadoras es simplemente un número, usualmente la cantidad de segundos desde un momento específico en el tiempo (como el primer instante de 1970, UTC, conocido como "Epoch"). Al convertir este número a una cadena de texto, estás haciendo la fecha interpretable por los humanos.

## Ver También:

- Documentación oficial de Fish Shell para `date`: https://fishshell.com/docs/current/commands.html#date
- Tutorial de Fish Shell: https://fishshell.com/docs/current/tutorial.html
- GNU Coreutils `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html

Recuerda que estos comandos pueden variar ligeramente en comportamiento dependiendo del sistema que estés utilizando (GNU/Linux, macOS, entre otros) y su configuración local.
