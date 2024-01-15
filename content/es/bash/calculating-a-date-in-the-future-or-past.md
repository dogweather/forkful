---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Bash: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Siempre es útil poder calcular la fecha en el futuro o en el pasado, ya sea por motivos personales o para tareas de programación. Con Bash, es posible hacerlo de manera sencilla y rápida utilizando algunos comandos básicos.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado, primero necesitamos tener en cuenta dos factores clave: la fecha actual y el número de días que queremos agregar o restar. Para obtener la fecha actual, podemos utilizar el comando `date` con el formato `%Y-%m-%d`, que nos dará la fecha en el formato año-mes-día. Para agregar o restar días, podemos utilizar el comando `date` nuevamente, pero esta vez con la opción `-d` seguida del número de días y la palabra "days".

Por ejemplo, si queremos calcular la fecha dentro de 10 días a partir de hoy, podemos usar el siguiente comando:

```Bash
date -d "10 days" +%Y-%m-%d
```

Esto nos dará como resultado la fecha en formato año-mes-día dentro de 10 días a partir de hoy. También podemos utilizar números negativos para restar días, por ejemplo:

```Bash
date -d "-5 days" +%Y-%m-%d
```

Esto nos dará como resultado la fecha en formato año-mes-día hace 5 días. Ahora, si en lugar de días queremos agregar o restar meses, podemos utilizar la opción `-d` seguida del número de meses y la palabra "months", por ejemplo:

```Bash
date -d "6 months" +%Y-%m-%d
```

Esto nos dará como resultado la fecha dentro de 6 meses a partir de hoy en formato año-mes-día. También podemos combinar el uso de días y meses, por ejemplo:

```Bash
date -d "2 months 5 days" +%Y-%m-%d
```

Esto nos dará como resultado la fecha dentro de 2 meses y 5 días a partir de hoy en formato año-mes-día.

## Profundizando

Existen muchas otras opciones y formatos que podemos utilizar con el comando `date` para calcular fechas en el futuro o en el pasado. Por ejemplo, podemos agregar o restar semanas, años, horas, minutos o incluso segundos. Además, podemos especificar fechas fijas en lugar de utilizar la fecha actual, por ejemplo:

```Bash
date -d "2022-01-01 3 days" +%Y-%m-%d
```

Esto nos dará como resultado la fecha dentro de 3 días a partir del 1 de enero del 2022 en formato año-mes-día. También podemos utilizar comandos pipe `|` para combinar el resultado de `date` con otros comandos, por ejemplo:

```Bash
date -d "2 days" +%Y-%m-%d | sed 's/0/1/'
```

Esto nos dará como resultado la fecha dentro de 2 días a partir de hoy, pero con la última cifra del día cambiada de 0 a 1.

## Vea también

- [Comandos básicos de Bash](https://www.gnu.org/software/bash/manual/html_node/Basic-Shell-Features.html)
- [Cómo utilizar el comando date](https://www.lifewire.com/format-date-output-linux-command-line-4032698)
- [Opciones avanzadas de date](https://www.computerhope.com/unix/udate.htm)