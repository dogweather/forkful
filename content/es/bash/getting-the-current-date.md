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

## ¿Qué y por qué?

Obtener la fecha actual es un proceso común en la programación en Bash. Esta acción permite a los programadores incluir la fecha en sus scripts, realizar tareas programadas o mantener un registro del tiempo en que se realizó una determinada acción. En resumen, obtener la fecha actual es una herramienta importante en el arsenal de un programador.

## ¡Cómo hacerlo!

Para obtener la fecha actual en Bash, podemos usar el comando `date`. Veamos algunos ejemplos de cómo usarlo.

```Bash
# Obtiene la fecha actual en formato mes/día/año
date +"%m/%d/%Y"

# Salida: 06/11/2021

# Obtiene la fecha actual en formato año-mes-día
date +"%Y-%m-%d"

# Salida: 2021-06-11

# Obtiene la fecha actual con la hora y el formato de 24 horas
date +"%F %T"

# Salida: 2021-06-11 16:42:23
```

Como se puede observar en los ejemplos anteriores, podemos especificar el formato deseado de la fecha utilizando el comando `date` junto con la opción `+`, seguida de las letras que representan el formato deseado. También podemos agregar información adicional, como la hora o el día de la semana, cambiando las letras en el formato.

## Profundizando

La obtención de la fecha actual en Bash se basa en el comando `date`, que está disponible en la mayoría de los sistemas operativos basados en Linux. Este comando se remonta a la década de 1970 y se ha mantenido como una herramienta útil en la programación y en la línea de comandos.

Alternativamente, también se puede utilizar el comando `time` para obtener la fecha y la hora actual en Bash. Sin embargo, este comando incluye información adicional como el uso de recursos del sistema y el tiempo de ejecución del comando.

A nivel de implementación, el comando `date` utiliza la zona horaria del sistema para mostrar la fecha y hora actual, y la opción `+` permite especificar el formato deseado en función de las letras disponibles. Además, se pueden combinar opciones para obtener información más detallada, como la fecha en formato RFC 822 (`date +"%a, %d %h %Y %T %z"`).

## Ver también

- [Bash Reference Manual - Date](https://www.gnu.org/software/bash/manual/html_node/Formatted-Output.html#Formatted-Output)
- [Unix Timestamp - The Ultimate Guide to Unix Timestamps](https://www.unixtimestamp.com/)