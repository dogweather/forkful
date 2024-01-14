---
title:    "Fish Shell: Convirtiendo una fecha en una cadena"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una fecha en una cadena de texto es una habilidad valiosa para cualquier programador que trabaje con shell en Fish. Te permitirá manipular y mostrar fechas de manera efectiva en tus scripts y comandos. Sigue leyendo para aprender cómo hacerlo.

## Cómo hacerlo

```Fish Shell
set date (date +%Y-%m-%d)
echo $date
```

Este sencillo código nos permite asignar la fecha actual ala variable "date" y luego imprimirlo en formato YYYY-MM-DD. Puedes personalizar el formato utilizando los símbolos específicos de fecha y hora disponibles en la documentación de Fish Shell.

```Fish Shell
set date (date +"%B %d, %Y")
echo $date
```

Este código imprimirá la fecha en formato mes, día y año, como por ejemplo "Mayo 05, 2020". También puedes usar estos comandos en combinación con otras funciones para obtener resultados más avanzados, como convertir una fecha específica en segundos desde epoch y luego convertir esos segundos en un formato legible.

## Profundizando

La conversión de fechas en cadenas de texto puede ser un proceso complejo y hay muchos recursos disponibles para ayudarte a profundizar en este tema. Puedes familiarizarte con el uso de variables de tiempo y fechas en Fish Shell, así como también aprender sobre cómo manipular y comparar fechas en tus scripts.

Si quieres llevar tus habilidades de conversión de fechas al siguiente nivel, también puedes explorar técnicas más avanzadas como la extracción de datos de múltiples formatos de fecha y la conversión de zona horaria.

## Ver también

Para obtener más información sobre cómo trabajar con fechas en Fish Shell, revisa estos recursos útiles:

- [Documentación de fecha y hora de Fish Shell (en inglés)](https://fishshell.com/docs/current/cmds/date.html)
- [Guía de uso de variables de tiempo en Fish Shell (en inglés)](https://scriptingosx.com/2020/04/using-fish-shell-variables-for-time-and-date/)
- [Tutorial de conversión de fechas en Fish Shell (en inglés)](https://techmonger.github.io/date-conversion-techniques-for-fish.html)