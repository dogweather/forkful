---
title:                "Bash: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo necesitamos convertir fechas en cadenas de texto. Esta conversión es importante para mostrar la fecha de una manera legible y comprensible para los usuarios, especialmente cuando trabajamos en proyectos que involucran diferentes zonas horarias y formatos de fecha.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en Bash, podemos usar el comando `date` junto con el formato deseado. Por ejemplo, para mostrar la fecha actual en formato dd/mm/yyyy, podemos escribir el siguiente código en Bash:

```Bash
date '+%d/%m/%Y'
```

Esto imprimirá la fecha actual en una cadena de texto con el formato deseado. El resultado puede ser algo así: 09/09/2021. También podemos especificar una fecha específica en lugar de la fecha actual utilizando el parámetro `-d` seguido de la fecha en formato ISO 8601. Por ejemplo:

```Bash
date '+%d/%m/%Y' -d "2021-05-22"
```

Esto imprimirá la fecha '22/05/2021'.

## Profundizando

El comando `date` cuenta con una amplia variedad de opciones de formato para mostrar diferentes componentes de una fecha, como el día, mes, año, hora, etc. Estos formatos se especifican mediante un símbolo de porcentaje junto con una letra que representa el componente deseado. Por ejemplo, `%d` para el día, `%m` para el mes y `%Y` para el año.

También podemos utilizar el comando `date` para manipular y realizar operaciones con fechas, como sumar o restar días, meses o años. Esto se logra utilizando el parámetro `-d` junto con una operación matemática. Por ejemplo, para sumar 5 meses a una fecha específica, podemos escribir:

```Bash
date -d "2021-09-09 +5 months" '+%d/%m/%Y'
```

Esto imprimirá la fecha '09/02/2022'. Para obtener más información sobre las diferentes opciones y formatos disponibles en el comando `date`, podemos consultar su manual escribiendo `man date` en la terminal.

## Ver también

- [Manual de `date` en GNU] (https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Guía de formato de fecha en Bash] (https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)
- [Manipulación de fechas en Bash] (https://www.tecmint.com/calculate-difference-between-two-dates-in-linux/)