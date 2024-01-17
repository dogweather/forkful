---
title:                "Obteniendo la fecha actual."
html_title:           "Fish Shell: Obteniendo la fecha actual."
simple_title:         "Obteniendo la fecha actual."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Obtener la fecha actual es una función importante en la programación, ya que permite a los desarrolladores crear aplicaciones dinámicas y automatizar tareas basadas en la fecha y hora. La fecha actual también se utiliza a menudo para registrar eventos o transacciones en una aplicación.

## Cómo:

Fish Shell proporciona una forma sencilla de obtener la fecha actual en un formato específico utilizando la función `date` seguida del formato deseado. Por ejemplo:

```
Fish Shell > date +%d-%m-%Y
15-05-2021
```

El código `%d` representa el día, `%m` el mes y `%Y` el año en formato de cuatro dígitos. Fish Shell también permite personalizar el formato de la fecha y hora según las necesidades del programador.

## Inmersión Profunda:

Obtener la fecha actual es una tarea común en la programación y hay varias formas de lograrlo. Algunos lenguajes de programación, como Python y JavaScript, tienen funciones incorporadas para obtener la fecha actual. En otros lenguajes, como C++ y Java, se pueden utilizar librerías externas para obtener la fecha actual.

Sin embargo, Fish Shell permite obtener la fecha actual de manera sencilla y directa, sin tener que importar librerías externas o escribir código adicional.

## Ver También:

- [Documentación oficial de Fish Shell para la función `date`](https://fishshell.com/docs/current/commands.html#date)
- [Ejemplos de uso de la función `date` en Fish Shell](https://fishshell.com/docs/current/tutorial.html#command-substitutions)