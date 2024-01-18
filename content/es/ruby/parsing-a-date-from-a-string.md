---
title:                "Extrayendo una fecha de una cadena."
html_title:           "Ruby: Extrayendo una fecha de una cadena."
simple_title:         "Extrayendo una fecha de una cadena."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Parsing de una fecha a partir de una cadena de texto es una tarea común en la programación. Consiste en convertir una cadena (string) que contiene una fecha en un formato específico, como "12 de mayo del 2020", en una estructura de datos que podamos manipular y utilizar en nuestro código. Los programadores realizan esta tarea porque a menudo necesitamos trabajar con fechas en nuestros programas, como calcular intervalos de tiempo o comparar fechas.

## Cómo hacerlo:
Podemos utilizar el método `Date.parse` de Ruby para realizar parsing de fechas a partir de una cadena de texto. Por ejemplo, si queremos convertir la fecha "12 de mayo del 2020" al objeto de fecha correspondiente, podemos hacer lo siguiente:

```Ruby
require 'date'
Date.parse("12 de mayo del 2020")
```

El resultado será un objeto de fecha correspondiente al 12 de mayo del 2020. También podemos especificar formatos de fecha personalizados utilizando la función `strptime`. Por ejemplo, si tenemos una cadena con el formato "12-05-2020", podemos utilizar `Date.strptime` de la siguiente manera:

```Ruby
Date.strptime("12-05-2020", "%d-%m-%Y")
```

El resultado será el mismo objeto de fecha correspondiente al 12 de mayo del 2020, pero esta vez especificando el formato de fecha que queríamos parsear.

## Profundizando:
Parsing de fechas a partir de cadenas de texto ha sido un problema común en la programación, especialmente antes de que los lenguajes de programación incluyeran métodos para realizar esta tarea de manera sencilla. En el pasado, los programadores tenían que escribir funciones personalizadas o utilizar librerías externas para realizar esta tarea. Sin embargo, con el lenguaje Ruby y su método `Date.parse`, podemos realizar esta tarea de manera eficiente y sencilla.

Otra alternativa es utilizar la librería `Chronic`, que nos permite parsear fechas en un lenguaje natural en lugar de tener que especificar un formato de fecha. Por ejemplo, podemos decir "mañana" o "hoy" y la biblioteca interpretará la fecha correspondiente.

En cuanto a la implementación, el método `Date.parse` de Ruby utiliza la clase `Date` y su función `parse` para realizar parsing de fechas a partir de una cadena de texto. También utiliza el estándar ISO 8601 para interpretar el formato de fecha por defecto.

## Mira también:
- Documentación del método `Date.parse` en la página oficial de Ruby
https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html#method-c-parse
- Documentación de la librería `Chronic` en la página oficial de RubyGems
https://rubygems.org/gems/chronic