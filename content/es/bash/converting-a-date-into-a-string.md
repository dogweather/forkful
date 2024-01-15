---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Bash: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena?

A veces, es necesario representar una fecha en formato de cadena para su uso en una aplicación o para su visualización en una interfaz de usuario. Aquí explicaremos cómo convertir una fecha en una cadena utilizando Bash.

## Cómo hacerlo

La conversión de una fecha en una cadena se puede realizar utilizando el comando `date` en Bash. A continuación se muestra un ejemplo de cómo se puede utilizar este comando para convertir la fecha actual en una cadena en formato "dd/mm/aaaa":

```Bash
fecha=$(date +"%d/%m/%Y")
echo "La fecha actual es $fecha"
```

Este código asigna la salida del comando `date` a la variable `fecha` y luego la imprime en la pantalla junto con un mensaje informativo. El resultado sería algo como esto:

```Bash
La fecha actual es 21/10/2021
```

También es posible especificar una fecha específica para convertirla en una cadena. Por ejemplo, si queremos convertir la fecha "13 de diciembre de 2021" en formato "dd-mm-aaaa", podemos hacerlo de la siguiente manera:

```Bash
fecha=$(date -d "13 Dec 2021" +"%d-%m-%Y")
echo "La fecha es $fecha"
```

El resultado sería:

```Bash
La fecha es 13-12-2021
```

Existen numerosas opciones disponibles para personalizar el formato de la cadena de fecha. Puedes encontrar una lista completa de estas opciones en la página de manual del comando `date`.

## Inmersión profunda

Ahora que sabemos cómo convertir una fecha en una cadena, veamos cómo se realizan realmente estas conversiones. En Bash, todas las fechas se almacenan en formato de tiempo EPoc (Unix time), que es un número entero que representa el número de segundos transcurridos desde la medianoche del 1 de enero de 1970. Al convertir una fecha a una cadena, el comando `date` realiza operaciones matemáticas para convertir el tiempo EPoc en una representación legible de la fecha.

En el código anterior, utilizamos la opción `+%d/%m/%Y` para especificar el formato de la cadena de fecha. Aquí está la explicación de lo que significan los símbolos:

- `%d` - día del mes con ceros iniciales (01-31)
- `%m` - mes con ceros iniciales (01-12)
- `%Y` - año con cuatro dígitos (por ejemplo, 2021)

Al jugar con las opciones, puedes crear una cadena de fecha en el formato que desees.

## Ver también

- [Página de manual del comando date en Linux](https://man7.org/linux/man-pages/man1/date.1.html)
- [Lista de formatos de fecha disponibles en date](https://wiki.bash-hackers.org/howto/formatting)