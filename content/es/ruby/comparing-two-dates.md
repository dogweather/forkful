---
title:                "Comparando dos fechas"
html_title:           "Ruby: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas en Ruby?

Comparar dos fechas en Ruby es una tarea común en la programación, especialmente cuando se trabaja con datos temporales. Al comparar dos fechas, puedes determinar si una fecha es anterior, posterior o igual a otra, lo que te permite realizar diferentes acciones en tu código según la comparación.

## Cómo comparar dos fechas en Ruby

```Ruby
# Crear dos objetos de fecha
date_1 = Date.new(2021, 5, 20)
date_2 = Date.new(2021, 6, 15)

# Comparar si date_1 es anterior a date_2
if date_1 < date_2
  puts "date_1 es anterior a date_2"
end

# Comparar si date_1 es posterior a date_2
if date_1 > date_2
  puts "date_1 es posterior a date_2"
end

# Comparar si date_1 es igual a date_2
if date_1 == date_2
  puts "date_1 es igual a date_2"
end
```

**Output:**
```
date_1 es anterior a date_2
```

En el código de ejemplo, hemos creado dos objetos de fecha utilizando el método `Date.new` y luego los comparamos utilizando los operadores de comparación `<` (menor que), `>` (mayor que) y `==` (igual a). Puedes utilizar estos operadores para comparar fechas tanto en formato de texto como en objetos de fecha.

## Profundizando en la comparación de dos fechas

Al comparar dos fechas en Ruby, es importante tener en cuenta algunos detalles. Primero, el método `Date#==` solo compara la fecha en sí, no la hora. Si necesitas comparar también la hora, puedes utilizar el método `Time#==` en su lugar. Además, también hay métodos predefinidos en Ruby para comparar si una fecha es anterior o posterior a otra basándose en diferentes unidades de tiempo, como días, meses o años. Por ejemplo, el método `Date#next_day` devuelve una nueva fecha que es el siguiente día del objeto de fecha.

Para obtener más información sobre cómo comparar fechas en Ruby, puedes consultar la documentación oficial [Date](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html) y [Time](https://ruby-doc.org/stdlib-2.7.1/libdoc/time/rdoc/Time.html).

## Ver también

- [Cómo trabajar con fechas y tiempos en Ruby](https://www.rubyguides.com/2015/05/working-with-dates-ruby/)
- [Documentación oficial de Date](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Documentación oficial de Time](https://ruby-doc.org/stdlib-2.7.1/libdoc/time/rdoc/Time.html)