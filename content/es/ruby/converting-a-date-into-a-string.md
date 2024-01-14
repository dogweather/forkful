---
title:                "Ruby: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué
Muchas veces en programación, necesitamos convertir una fecha en un formato en particular, como una cadena de texto. Esto puede ser útil para mostrar fechas en un formato más legible, para realizar búsquedas y comparaciones de fechas, o para almacenar la fecha en una base de datos.

## Cómo hacerlo
Para convertir una fecha en una cadena de texto en Ruby, podemos usar el método `strftime`. Este método toma una fecha y un formato de conversión como argumentos y devuelve una cadena de texto con la fecha formateada. Por ejemplo:

```Ruby
date = Time.new(2021, 4, 15)
puts date.strftime("%d/%m/%Y")
# Salida: 15/04/2021
```

En este ejemplo, creamos una fecha utilizando la clase `Time` y luego utilizamos `strftime` con el formato `"%d/%m/%Y"` para obtener la fecha en el formato de día/mes/año.

Algunos formatos de conversión comunes son:

- `%d` para el día del mes (01-31)
- `%m` para el mes (01-12)
- `%Y` para el año (con 4 dígitos)
- `%H` para la hora (00-23)
- `%M` para los minutos (00-59)
- `%S` para los segundos (00-59)

Puedes encontrar una lista completa de formatos de conversión [aquí](https://www.rubyguides.com/2015/10/ruby-time/). Además, también puedes usar el método `strftime` en objetos `DateTime` y `Date` para convertir fechas en cadena de texto.

## Profundizando
Si quieres profundizar en la conversión de fechas en Ruby, hay algunos conceptos que puedes explorar. Algunos de ellos son:

- Formatos locales: Puedes utilizar el método `setlocale` para establecer un idioma y región específicos para formatear las fechas en consecuencia.
- Zonas horarias: Puedes utilizar la gema `TZInfo` para manejar diferentes zonas horarias al convertir fechas en cadena de texto.
- Personalización de formatos: Además de los formatos de conversión predefinidos, también puedes crear tus propios formatos personalizados utilizando el método `DateTime#strftime` o `Time#strftime`.

## Ver también
- [Documentación de Ruby sobre el método `strftime`](https://ruby-doc.org/core-2.7.2/Time.html#method-i-strftime)
- [Tutorial de Ruby en español](http://tutoriales.programacion-entornos.net/2019/02/06/ruby-programacion/)
- [Gema TZInfo](https://tzinfo.github.io/)