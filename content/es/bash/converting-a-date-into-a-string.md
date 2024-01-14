---
title:                "Bash: Convirtiendo una fecha en una cadena"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una fecha en una cadena de caracteres es una tarea común en la programación. Al convertir una fecha en una cadena, se puede personalizar el formato y presentación de la fecha según las necesidades del usuario.

## Cómo hacerlo

En Bash, se puede utilizar el comando `date` para obtener la fecha actual en diferentes formatos. Para convertir la fecha en una cadena de caracteres, se puede utilizar el siguiente código:

```Bash
fecha=$(date +"%d %b %Y") 
echo "La fecha actual es: $fecha"
```

Esto mostrará la fecha actual en formato día-mes-año ("dd mmm yyyy"). El código utiliza el parámetro `+"%d %b %Y"` para especificar el formato deseado, donde `%d` representa el día, `%b` el mes abreviado y `%Y` el año en cuatro dígitos. 

Pero ¿qué pasa si se quiere utilizar un formato diferente o incluir la hora en la cadena de caracteres? Para ello, se pueden utilizar los siguientes parámetros:

- `%a` para el día abreviado de la semana
- `%A` para el día completo de la semana
- `%m` para el número del mes (con ceros iniciales)
- `%M` para los minutos
- `%H` para las horas en formato de 24 horas
- `%I` para las horas en formato de 12 horas
- `%p` para indicar si es AM o PM

Por ejemplo, para obtener la fecha y hora actuales en formato día-mes-año, seguido de la hora en formato de 12 horas, se puede utilizar el siguiente código:

```Bash
fecha=$(date +"%d %b %Y, %I:%M:%S %p") 
echo "La fecha y hora actuales son: $fecha"
```

La salida sería algo como "24 Mar 2021, 09:15:02 AM". 

## Profundizando

Para una mayor personalización, se pueden consultar en la documentación de `date` los diferentes parámetros disponibles para formatear la fecha y hora. Además, se pueden utilizar otros comandos de Bash, como `cut` y `grep`, para extraer solo la parte de la fecha o la hora que se desee. 

También se pueden utilizar variables para almacenar la fecha y hora convertidas en cadenas y utilizarlas en diferentes partes de un script. Otra opción es utilizar la función `strftime` para convertir una fecha en un formato específico.

Convertir una fecha en una cadena de caracteres también puede ser útil en aplicaciones web, donde se pueden mostrar las fechas en diferentes idiomas y formatos según las preferencias del usuario. 

## Ver también

- Documentación de `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation
- Tutorial de Bash: https://www.hostinger.es/tutoriales/tutorial-bash/
- Ejemplos de formatos de fecha y hora: https://linuxcommandlibrary.com/man/date.html