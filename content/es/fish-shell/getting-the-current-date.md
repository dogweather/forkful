---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:14:31.803888-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Obtener la fecha actual significa acceder a la información de tiempo del sistema en el que estás trabajando. Los programadores lo hacen para realizar tareas como generar timestamps, programar eventos o simplemente registrar cuándo suceden ciertas acciones.

## Cómo hacerlo:

Para obtener la fecha y hora actuales en Fish Shell, usas el comando `date`. Aquí tienes unos ejemplos:

```Fish Shell
# Obtener la fecha y hora completa
date

# Formato personalizado: año-mes-día
date "+%Y-%m-%d"

# Solo la hora
date "+%H:%M:%S"
```

Ejemplo de salida:

```Fish Shell
# Salida típica para fecha y hora completa
Tue Mar 14 21:53:02 CET 2023

# Salida para el formato personalizado
2023-03-14

# Salida para la hora
21:53:02
```

## Deep Dive

Históricamente, los comandos para obtener la fecha y hora del sistema vienen de los tiempos de Unix y han permanecido bastante consistentes a través de los años. En Fish Shell, al igual que en otras terminales Unix, utilizas `date` para estas operaciones. Existen alternativas como `strftime` con la función `math`, que hace, por ejemplo, operaciones de cálculo con fechas, aunque para usos básicos `date` es más que suficiente.

Implementar la obtención de la fecha en scripts puede ser útil para crear archivos de logs, definir timestamps de cuando se ejecutan ciertas tareas o para sincronizar eventos. En Fish Shell, esto se hace de manera sencilla con el comando `date` y sus opciones de formato, permitiendo una gran flexibilidad en cómo presentar la información.

## Ver También

- La página de manual de `date` para explorar más opciones de formato: [Man page of DATE](https://linux.die.net/man/1/date)
- Documentación oficial de Fish para entender mejor cómo personalizar tu shell y sus comandos: [Fish Documentation](https://fishshell.com/docs/current/index.html)
- Tutoriales de Fish Shell para comenzar con scripting y tips: [Fish Shell Tutorials](https://fishshell.com/docs/current/tutorial.html)