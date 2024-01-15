---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Python: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué 

Convertir una fecha en una cadena de texto (string) es una habilidad importante en programación, ya que permite representar una fecha de manera legible para el usuario. Esto puede ser útil para mostrar fechas en interfaces de usuario, generar informes o trabajar con bases de datos.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en Python, se puede utilizar el método `strftime()` del objeto `datetime`. Veamos un ejemplo:

```Python
import datetime

# Creamos un objeto datetime con la fecha actual 
hoy = datetime.datetime.now()

# Utilizamos el método strftime() para convertir la fecha en una cadena de texto con el formato deseado
fecha_str = hoy.strftime("%d/%m/%Y")

# Imprimimos la fecha como una cadena de texto
print(fecha_str)

# Output: 06/04/2021
```

En el ejemplo anterior, utilizamos el formato `"dd/mm/aaaa"` en el método `strftime()` para obtener la fecha en formato día/mes/año. Sin embargo, existen otros códigos de formato que se pueden utilizar para representar la fecha de diferentes formas. Aquí hay algunos ejemplos:

- `%d`: día del mes (01-31)
- `%m`: mes (01-12)
- `%Y`: año (ej: 2021)
- `%y`: año sin el siglo (ej: 21)
- `%A`: día completo de la semana (ej: lunes)
- `%B`: mes completo (ej: enero)
- `%b`: abreviación del mes (ej: ene)
- `%H`: hora en formato 24 horas (00-23)
- `%I`: hora en formato 12 horas (01-12)
- `%M`: minutos (00-59)
- `%S`: segundos (00-59)

Puedes combinar estos códigos de formato para obtener la representación de la fecha que necesites. Por ejemplo, si quieres mostrar la fecha con el nombre del mes abreviado y el año en formato de dos dígitos, puedes utilizar el siguiente código: `fecha.strftime("%d %b %y")`.

## Detalles más técnicos

El método `strftime()` acepta otros argumentos además del código de formato, como `tzinfo` para especificar la zona horaria o `utc` para obtener la hora en formato UTC. Además, también se puede usar el método `strptime()` para convertir una cadena de texto en un objeto `datetime`. Este método acepta una cadena de texto y un código de formato, y devuelve un objeto `datetime` con la fecha representada en la cadena de texto.

## Ver también

- [Documentación oficial de Python sobre el formato de fecha y hora](https://docs.python.org/es/3/library/datetime.html#strftime-and-strptime-behavior)
- [Tutorial de Real Python sobre el formato de fecha y hora en Python](https://realpython.com/python-datetime/)
- [Stack Overflow: Preguntas sobre cómo convertir una fecha en una cadena de texto en Python](https://stackoverflow.com/questions/tagged/python+datetime+string-conversion)