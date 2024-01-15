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

## ¿Por qué?

¿Te has preguntado alguna vez cómo convertir una fecha en Ruby a un formato de cadena de texto? Puede que necesites hacerlo para mostrar la fecha en un formato específico o para guardarla en una base de datos. En este artículo, te enseñaremos cómo hacerlo de manera sencilla y eficiente.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en Ruby, utilizamos el método `strftime`, que significa "string from time". Este método permite especificar el formato en el que queremos mostrar la fecha. Aquí tienes un ejemplo de código:

```ruby
fecha = Time.new(2021, 9, 15) # Crear una fecha de ejemplo
puts fecha.strftime("%d/%m/%Y") # La fecha se muestra en formato DD/MM/YYYY
```

El resultado sería:

```
15/09/2021
```

El primer argumento del método `strftime` es una cadena con el formato deseado. Aquí hay algunos de los caracteres más comunes que puedes utilizar:

- `%d`: día del mes con dos dígitos
- `%m`: mes con dos dígitos
- `%Y`: año con cuatro dígitos
- `%H`: hora con formato 24 horas
- `%M`: minuto con dos dígitos
- `%S`: segundo con dos dígitos

Si quieres ver una lista completa de los caracteres disponibles, puedes consultar la documentación oficial de Ruby.

## Profundizando

El método `strftime` utiliza un objeto del tipo `Time` para convertirlo en una cadena de texto. Este objeto contiene toda la información de la fecha y hora, como año, mes, día, hora, minuto y segundo. Al especificar el formato que queremos en el método `strftime`, estamos diciéndole a Ruby qué información debe incluir en la cadena de texto.

También podemos utilizar el método `strftime` con objetos del tipo `Date` y `DateTime`, que tienen la misma estructura que el objeto `Time`.

## Ver también

- [Documentación oficial de Ruby sobre strftime](https://docs.ruby-lang.org/en/3.0.0/Time.html#method-i-strftime)
- [Artículo de Ruby para principiantes sobre objetos Time, Date y DateTime](https://www.freecodecamp.org/news/ruby-time-date-datetime/)
- [Guía de formateo de fechas en Ruby](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-formatting-numbers-and-strings-u/articles/ruby-formatting-date)