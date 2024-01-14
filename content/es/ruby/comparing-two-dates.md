---
title:    "Ruby: Comparando dos fechas"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas en Ruby?

Comparar dos fechas puede ser una tarea común en la programación. Puede ser útil para determinar si una fecha está antes o después de otra, o para calcular la diferencia de tiempo entre dos fechas. En Ruby, hay varias formas en las que se puede realizar esta comparación.

## Cómo hacerlo:

Para comparar dos fechas en Ruby, primero necesitamos crear objetos de tipo Date o Time. Luego, podemos utilizar los operadores de comparación ">" (mayor que) y "<" (menor que) para determinar si una fecha es anterior o posterior a otra. También podemos utilizar el método `between?` para verificar si una fecha se encuentra entre dos fechas específicas.

Por ejemplo, si queremos comparar las fechas de hoy y mañana, podemos crear los objetos de tipo Date y utilizar el operador ">" de la siguiente manera:

````Ruby

hoy = Date.today 
mañana = Date.today + 1

if hoy > mañana 
puts "#{hoy} viene después de #{mañana}"
else 
puts "#{hoy} viene antes de #{mañana}"
end
````

Esto nos dará la siguiente salida: "2021-10-05 viene antes de 2021-10-06".

También podemos usar los métodos `day`, `month` y `year` para comparar fechas basándonos en el día, mes o año específico. Por ejemplo, si queremos comparar si un evento ocurre en el mismo mes que hoy, podemos hacer lo siguiente:

````Ruby
# Creando un objeto de tipo Time para representar una fecha concreta, en este caso, el 5 de octubre de 2021
evento = Time.new(2021, 10, 5)

if Date.today.month == evento.month
puts "El evento ocurre en el mismo mes que hoy"
end
````

## Profundizando en la comparación de fechas

Además de los operadores de comparación y los métodos mencionados anteriormente, Ruby también ofrece varios otros métodos útiles para comparar fechas. Algunos de ellos incluyen el método `past?` para determinar si una fecha ya pasó, `future?` para saber si una fecha aún no ha llegado, `next_month` para obtener la fecha del próximo mes y `beginning_of_day` para obtener el inicio del día en una fecha específica.

También es importante tener en cuenta que los objetos de tipo Date y Time también tienen métodos específicos para comparar fechas en diferentes formatos, como `strftime` para convertir una fecha a un formato específico y `parse` para convertir una fecha en una cadena a un objeto Date o Time.

¡Experimenta con diferentes métodos y formatos para comparar fechas en Ruby y descubre cuál funciona mejor para ti y tus proyectos!

## Ver también

- [Documentación de Ruby sobre la clase Date](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Date.html)
- [Documentación de Ruby sobre la clase Time](https://ruby-doc.org/core-3.0.1/Time.html)
- [Tutorial de comparación de fechas en Ruby](https://www.rubyguides.com/2016/12/compare-dates-in-ruby/)

¡Ahora estás listo para comparar fechas en tus proyectos de Ruby! ¡Empieza a jugar con diferentes métodos y encuentra la mejor forma para ti!