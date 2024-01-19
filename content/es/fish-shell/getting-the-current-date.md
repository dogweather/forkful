---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Obtener la fecha actual en programación es capturar el día, mes y año actuales en tiempo real. Los programadores lo hacen para registrar eventos o para estampar marcas de tiempo (timestamps).

## Cómo hacerlo:
Aquí te muestro un sencillo trozo de código en Fish Shell para obtener la fecha actual:

```fish
set -l fecha (date "+%d/%m/%Y")
echo $fecha
```

Al ejecutar este bloque de código, el resultado será algo como esto:

```fish
22/02/2023
```

## Más a fondo:
**Contexto histórico:** Los comandos para obtener la fecha y la hora son tan antiguos como los propios sistemas UNIX, has sido una parte crucial de los scripts y la programación en shell desde sus inicios.

**Alternativas:** Existen diferentes formas de lograr lo mismo, algunas de las alternativas incluyen el uso de diferentes formatos de fecha (`"%m-%d-%Y"`, `"%Y%m%d"`), o incluso combinar la fecha con la hora actual.

**Detalles de implementación:** En Fish Shell, usamos el comando `date` que es una utilidad estándar de UNIX. El formato de la fecha se puede personalizar utilizando códigos como `%d` para el día, `%m` para el mes y `%Y` para el año.

## Ver también:
1. [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/index.html)
2. [Manual de UNIX `date`](https://man7.org/linux/man-pages/man1/date.1.html)
3. [Documentación de código de formato `date`](https://www.gnu.org/software/coreutils/manual/html_node/Date-input-formats.html)