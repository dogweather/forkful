---
title:                "Obteniendo la fecha actual"
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Si estás programando en Bash, una de las tareas comunes que puedes necesitar hacer es obtener la fecha actual. Esto puede ser útil para, por ejemplo, crear backups de tus archivos con una marca de tiempo.

## Cómo hacerlo

```Bash
# Obtener la fecha actual en formato mm/dd/aaaa
date +"%m/%d/%Y"

# Obtener el día de la semana
date +"%A"

# Obtener la fecha y hora en formato yyyy-mm-dd hh:mm:ss
date +"%F %T"
```

Ejemplo de salida:

```
06/14/2021
Monday
2021-06-14 14:30:45
```

## Deep Dive

La herramienta `date` en Bash te permite personalizar el formato de la fecha y hora que se muestra. Por ejemplo, puedes usar el formato `%b` para obtener el mes abreviado (por ejemplo, "Jun") o `%H` para obtener la hora en formato de 24 horas (por ejemplo, "14" para las 2 PM). Puedes encontrar más opciones de formato en la página de manual de `date` utilizando el comando `man date` en tu terminal.

## Ver también

- [Cómo trabajar con fechas en Bash](https://www.linuxjournal.com/content/working-dates-bash)
- [Manual de `date` en Linux](https://man7.org/linux/man-pages/man1/date.1.html)
- [Guía de Bash en español](http://www.mclibre.org/consultar/bash/index.html)