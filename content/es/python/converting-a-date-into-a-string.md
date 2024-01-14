---
title:    "Python: Conversión de una fecha a una cadena de caracteres"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Por qué

En la programación de Python, a menudo nos encontramos con la necesidad de convertir una fecha en una cadena de texto. Esto puede ser útil para mostrar fechas en un formato específico o para realizar cálculos de tiempo.

##Cómo hacerlo

Para realizar esta conversión, necesitamos importar el módulo de datetime en Python y utilizar su función "strftime" que permite formatear una fecha y hora en una cadena de texto.

```Python
import datetime

fecha_actual = datetime.datetime.now()

fecha_cadena = fecha_actual.strftime("%d/%m/%Y")

print(fecha_cadena)

#output: 13/10/2021
```

En este ejemplo, hemos utilizado el formato "%d/%m/%Y" para obtener la fecha actual en formato dd/mm/aaaa. Sin embargo, el formato puede variar dependiendo de nuestras necesidades.

##Profundizando

Al utilizar la función "strftime" podemos especificar diferentes parámetros para obtener diferentes formatos de fecha y hora. Algunos de los más comunes son:

- %d: día del mes (01-31)
- %m: mes (01-12)
- %b: mes como nombre abreviado (Jan-Dec)
- %B: mes como nombre completo (January-December)
- %Y: año con cuatro dígitos (2021)
- %y: año con dos dígitos (21)
- %H: hora en formato de 24 horas (00-23)
- %I: hora en formato de 12 horas (01-12)
- %M: minutos (00-59)
- %S: segundos (00-59)
- %p: AM o PM
- %A: día de la semana como nombre completo (Monday-Sunday)
- %a: día de la semana como nombre abreviado (Mon-Sun)

Este es solo un pequeño ejemplo de los formatos que se pueden utilizar. Existen muchas más opciones y es importante consultar la documentación de datetime para conocer todas las posibilidades.

##Ver también

- [Documentación de datetime en Python](https://docs.python.org/es/3/library/datetime.html)
- [Guía para formatear fechas en Python](https://www.programiz.com/python-programming/datetime/strftime)
- [Ejemplos prácticos de conversión de fechas en Python](https://www.w3schools.com/python/python_datetime.asp)