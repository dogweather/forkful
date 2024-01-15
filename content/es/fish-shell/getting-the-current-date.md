---
title:                "Obteniendo la fecha actual"
html_title:           "Fish Shell: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué obtener la fecha actual con Fish Shell?

La obtención de la fecha actual es una tarea común en programación y puede ser útil para realizar tareas como registrar la fecha y hora de ejecución de un script o para mostrar la fecha actual en un sistema. Gracias a su sintaxis sencilla, Fish Shell se convierte en una forma fácil y rápida de obtener la fecha actual en tu sistema.

## Cómo hacerlo

Para obtener la fecha actual en Fish Shell, puedes utilizar el comando `date` seguido del formato deseado:

```
Fish Shell
$ date "+%d/%m/%Y"
18/06/2021
```

El código anterior retornará la fecha actual en el formato día/mes/año. También puedes obtener solo el día o el mes utilizando `%d` para el día y `%m` para el mes.

```
Fish Shell
$ date "+%d"
18 
$ date "+%m"
06
```

Además de las opciones de día y mes, puedes obtener la hora actual utilizando `%H` para la hora y `%M` para los minutos.

```
Fish Shell
$ date "+%H:%M"
13:15
```

Puedes combinar diferentes opciones para obtener la fecha y hora completa en el formato deseado.

```
Fish Shell
$ date "+%d/%m/%Y %H:%M:%S"
18/06/2021 13:20:30
```

## Profundizando más

Además de los formatos predefinidos, puedes personalizar completamente el formato de la fecha utilizando el comando `date` con la opción `-f` seguido del formato deseado.

```
Fish Shell
$ date -f "%A, %d de %B de %Y"
viernes, 18 de junio de 2021
```

Para obtener más información sobre los formatos de fecha y cómo personalizarlos, puedes consultar la página de ayuda de Fish Shell utilizando el comando `help date`.

## Ver también

- [Documentación de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial de Fish Shell en español](https://manueljtejada.com/tutoriales/introduccion-a-fish-shell/)