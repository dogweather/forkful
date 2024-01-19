---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Obtener la fecha actual en programación es simplemente recuperar el día, mes y año en el momento en que se ejecuta el código. Los programadores hacen esto para rastrear eventos de tiempo y proporcionar sellos de tiempo relevantes.

## ¿Cómo hacerlo?

Puedes obtener la fecha actual en Bash usando el comando `date`. Aquí tienes un ejemplo básico:

```Bash
#!/bin/bash
# Obteniendo la fecha actual
fecha=$(date)
echo "La fecha actual es: $fecha"
```

Si ejecutas este script, la salida se verá así:

```Bash
La fecha actual es: Vie 20 Ago 2021 14:50:27 CEST
```

En este caso, `date` devuelve la fecha y hora actuales.

¿Solo quieres la fecha? No hay problema. Puedes formatearlo:

```Bash
#!/bin/bash
# Obteniendo solo la fecha
fecha=$(date +%Y-%m-%d)
echo "La fecha actual es: $fecha"
```

Y la salida será:

```Bash
La fecha actual es: 2021-08-20
```

## Profundizando

Aunque el comando `date` es la forma más básica y directa de obtener la fecha en Bash, tiene una larga historia en Unix y sistemas tipo Unix. Existe desde los primeros días de Unix y ha sido honrado durante mucho tiempo por su flexibilidad y potencia.

Existen algunas alternativas a `date`. Por ejemplo, `printf` con `%T` puede devolver la hora actual y `perl` tiene un módulo `Time::Piece` incorporado que recoge la fecha y la hora actuales.

La implementación de `date` en Bash está influenciada por el estándar POSIX. En versiones anteriores de Bash, podrías obtener una salida de error si intentas utilizar características no POSIX.

## Véase también

Para obtener más información sobre el manejo de fechas y horas en Bash, echa un vistazo a estos recursos:

- [Bash Date Command (ss64.com)](https://ss64.com/bash/date.html)
- [Date Command In Linux (linuxize.com)](https://linuxize.com/post/date-command-in-linux/)
- [How To Get Current Date And Time In Bash (cyberciti.biz)](https://www.cyberciti.biz/faq/unix-linux-getting-current-date-in-bash-ksh-shell-script/)