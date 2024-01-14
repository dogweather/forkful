---
title:                "Ruby: Convirtiendo una fecha en una cadena"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador principiante en Ruby, es posible que te hayas enfrentado al desafío de convertir fechas en cadenas de texto. Este proceso es común en la programación cuando necesitas mostrar una fecha en un formato específico o almacenarla en una base de datos. Afortunadamente, Ruby ofrece una variedad de métodos y opciones para ayudarte en esta tarea.

## Cómo convertir una fecha en una cadena de texto

Hay varias formas de convertir una fecha en una cadena de texto en Ruby, pero aquí te mostraremos la forma más sencilla y popular. Utilizaremos el método `strftime` para formatear la fecha y luego convertirla en una cadena de texto.

```
today = Time.now
puts today.strftime("%m/%d/%Y")
# Output: 01/08/2021 (si hoy es 8 de enero de 2021)
```
En este ejemplo, utilizamos la variable `today` para almacenar la fecha actual y luego llamamos al método `strftime` con el formato deseado como argumento. En este caso, utilizamos `%m` para el número del mes, `%d` para el día del mes y `%Y` para el año. Puedes consultar la documentación de Ruby para ver otros formatos de fecha disponibles.

Otra forma de convertir una fecha en una cadena de texto es utilizando el método `to_s`.

```
date = Date.new(2021, 1, 8)
puts date.to_s
# Output: 2021-01-08
```

Este método también te permite especificar un formato utilizando `strftime` como argumento.

## Profundizando en la conversión de fechas a cadenas de texto

Si deseas conocer más sobre el proceso de conversión de fechas a cadenas de texto en Ruby, hay algunos detalles importantes que debes tener en cuenta.

En primer lugar, cuando utilizas `strftime` para formatear una fecha, el resultado siempre será una cadena de texto. Sin embargo, cuando utilizas el método `to_s`, el resultado dependerá del objeto de fecha que estés utilizando. Por ejemplo, si utilizas una instancia de la clase `Date`, el resultado será una fecha en formato ISO (AAAA-MM-DD).

Otro punto a considerar es que cuando utilizas diferentes formatos con `strftime`, puede haber una pequeña diferencia en el resultado. Por ejemplo, `%m` devolverá un mes con ceros a la izquierda (por ejemplo, 01 para enero), mientras que `%-m` devolverá el mes sin ceros a la izquierda (por ejemplo, 1 para enero).

En resumen, convertir una fecha en una cadena de texto en Ruby puede parecer un poco confuso al principio, pero con un poco de práctica, podrás dominar este proceso en poco tiempo.

## Ver también

- [Documentación de Ruby sobre strftime](https://ruby-doc.org/core-2.7.2/Time.html#method-i-strftime)
- [Más sobre la conversión de fechas en cadenas de texto en Ruby](https://stackoverflow.com/questions/18658781/how-to-convert-a-date-object-to-a-string-in-ruby)