---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Ruby: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Convertir una fecha en una cadena de texto es una manera de representar una fecha en un formato legible para los humanos. Los programadores a menudo hacen esto para mostrar la fecha en una interfaz de usuario o para almacenarla en una base de datos de una manera fácilmente comprensible.

## Cómo:
```Ruby
# Ejemplo 1: Convertir una fecha en una cadena de texto
date = Date.new(2021, 10, 12)
puts date.to_s # Output: "2021-10-12"
# Se puede usar el método ".strftime" para especificar el formato deseado
puts date.strftime("%d/%m/%Y") # Output: "12/10/2021"

# Ejemplo 2: Convertir una fecha y hora en una cadena de texto
time = Time.new(2021, 10, 12, 10, 30, 00)
puts time.to_s # Output: "2021-10-12 10:30:00"
# También se puede usar el método ".strftime" para especificar el formato deseado
puts time.strftime("%d/%m/%Y %H:%M:%S") # Output: "12/10/2021 10:30:00"
```

## Profundizando:
La conversión de una fecha en una cadena de texto es una práctica común en la programación debido a la necesidad de mostrar fechas en una forma legible para los usuarios. Antes de la creación de sistemas de procesamiento de tiempo, las fechas se almacenaban y manipulaban en formato de cadena de texto. Sin embargo, con la evolución de la tecnología, surgieron nuevas formas de representar y manipular fechas, como objetos de fecha y hora en lenguajes de programación modernos.

Un método alternativo para convertir una fecha en una cadena de texto es utilizar el método ".to_s" en lugar de ".strftime". Esto producirá una cadena de texto utilizando el formato predeterminado del lenguaje de programación. Además, la función "DateTime.parse()" también puede usarse para convertir una cadena de texto en un objeto de fecha.

En Ruby, las fechas se almacenan como números enteros que representan la cantidad de días desde el 1 de enero de 4713 a.C. Este sistema de calendario se llama Calendario Juliano. Sin embargo, en la mayoría de los casos, las fechas se muestran en el formato Gregoriano, que se usa en la mayoría de los países occidentales.

## Ver también:
- Documentación oficial de Ruby sobre la clase Date: https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html
- Documentación oficial de Ruby sobre la clase Time: https://ruby-doc.org/core-2.7.0/Time.html
- Artículo de Medium sobre diferentes formas de trabajar con fechas en Ruby: https://medium.com/rubycademy/manipulando-fechas-en-ruby-6ab88bb3d8d