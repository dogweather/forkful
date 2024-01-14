---
title:                "Ruby: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

Comparar fechas en un programa es una tarea común que puede ayudar a determinar la duración de un evento o calcular el tiempo transcurrido entre dos fechas específicas.

## Cómo hacerlo

Usando Ruby, podemos utilizar el método `compare` para comparar dos fechas y obtener un resultado numérico basado en la diferencia entre ellas. Veamos un ejemplo de cómo hacerlo:

```Ruby
fecha_1 = Time.new(2021, 9, 1)
fecha_2 = Time.new(2021, 8, 1)

resultado = fecha_1.compare(fecha_2)
puts resultado
```

Este código imprimirá "1" ya que la fecha 1 es más reciente que la fecha 2. Además, también podemos utilizar el método `between?` para determinar si una fecha se encuentra entre otras dos fechas. Veamos otro ejemplo:

```Ruby
fecha = Time.now
fecha_inicio = Time.new(2021, 1, 1)
fecha_fin = Time.new(2021, 12, 31)

if fecha.between?(fecha_inicio, fecha_fin)
  puts "La fecha actual se encuentra entre las fechas elegidas"
end
```

Este código imprimirá el mensaje si la fecha actual está entre el 1 de enero y el 31 de diciembre del año actual.

## Profundizando

Al comparar fechas en Ruby, es importante tener en cuenta que los resultados se basan en la diferencia de tiempo en segundos. Por lo tanto, es importante estar atentos a las zonas horarias y ajustarlas adecuadamente para obtener resultados precisos. Además, Ruby también ofrece otros métodos para comparar fechas, como `eql?` y `==` que pueden ser útiles en diferentes situaciones.

Por otro lado, es importante tener en cuenta que Ruby también nos permite comparar fechas en formato string utilizando el método `Date.parse`. Esto puede ser útil para comparar fechas ingresadas por el usuario o desde un archivo externo.

## Ver también

- [Documentación de Ruby para el método "compare"](https://ruby-doc.org/core-3.0.1/Time.html#method-i-compare)
- [Documentación de Ruby para el método "between?"](https://ruby-doc.org/core-3.0.1/Time.html#method-i-between-3F)
- [Artículo sobre cómo comparar fechas en Ruby](https://www.geeksforgeeks.org/how-to-compare-dates-in-ruby/)