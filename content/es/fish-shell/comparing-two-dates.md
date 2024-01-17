---
title:                "Comparando dos fechas"
html_title:           "Fish Shell: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

¡Hola personas programadoras! ¿Necesitan comparar diferentes fechas en sus programas? ¡No se preocupen más! En este artículo, les mostraremos cómo hacerlo de manera sencilla con Fish Shell.

## ¿Qué y por qué?
Comparar dos fechas es una forma de determinar si una fecha es anterior, igual o posterior a otra. Los programadores suelen hacer esto para organizar y clasificar datos, o para realizar operaciones basadas en el tiempo.

## Cómo hacerlo:
¡Es muy fácil hacer esto con Fish Shell! Sólo necesitan utilizar el comando `date` seguido de las fechas que desean comparar entre comillas.

```Fish Shell
date "12/03/2021" "25/05/2021"
```
La salida será un número negativo si la primera fecha es anterior a la segunda, cero si son iguales, y un número positivo si la primera fecha es posterior a la segunda.

## Deep Dive:
Este proceso de comparación de fechas se basa en la expansión de variables de Fish Shell, que convierte las fechas en segundos desde el año 1970. Otras alternativas para comparar fechas en Fish Shell incluyen el uso del comando `strftime` o de la herramienta `dateutils`.

## Ver también:
- Documentación oficial de Fish Shell: [Comparing Dates](https://fishshell.com/docs/current/cmds/date.html).
- [Expansión de variables en Fish Shell](https://fishshell.com/docs/current/tutorial.html#variables).