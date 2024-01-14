---
title:    "Ruby: Comparando dos fechas"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Por qué comparar dos fechas en Ruby

Comparar dos fechas puede ser una tarea común en la programación en Ruby. Puede ser útil para verificar si una fecha es anterior o posterior a otra, o para determinar cuánto tiempo ha pasado entre dos eventos. En esta publicación, exploraremos cómo comparar fechas en Ruby y profundizaremos en su funcionamiento.

## Cómo hacerlo

Existen varias formas de comparar dos fechas en Ruby. Una de las formas más sencillas es utilizando el método `Date#<=>`. Este método compara dos objetos fecha y devuelve -1 si la primera fecha es anterior a la segunda, 0 si son iguales y 1 si la primera fecha es posterior a la segunda. Veamos un ejemplo:

```Ruby
date1 = Date.new(2020, 10, 1)
date2 = Date.new(2020, 12, 1)

puts date1 <=> date2
```

Este código imprimirá -1, ya que la fecha 1 es anterior a la fecha 2. También podemos utilizar los operadores de comparación `<`, `<=`, `==`, `>=` y `>`. Veamos otro ejemplo:

```Ruby
date1 = Date.new(2020, 11, 20)
date2 = Date.new(2020, 11, 21)

puts date1 < date2 # Imprime true
puts date1 >= date2 # Imprime false
```

Otra forma de comparar fechas es utilizando el método `Date#eql?`, que devuelve true si dos fechas son iguales en año, mes y día. También podemos utilizar `Date#between?` para determinar si una fecha se encuentra entre dos fechas dadas.

## Profundizando

Ahora que sabemos cómo comparar fechas en Ruby, profundicemos en cómo funciona esto detrás de escena. Las fechas en Ruby son en realidad objetos del tipo `Date`, que tienen métodos y atributos específicos para realizar operaciones relacionadas con fechas.

Cuando utilizamos los operadores de comparación, lo que realmente está sucediendo es que Ruby llama al método correspondiente (`<`, `<=`, `==`, `>=` o  `>`) en el objeto fecha que está a la izquierda del operador, pasando como argumento el objeto fecha a la derecha del operador. Este método compara los valores de año, mes y día de cada fecha y devuelve el resultado según corresponda.

Al utilizar el método `Date#between?`, Ruby compara si la fecha dada está entre las dos fechas dadas. Este método toma en cuenta la posición relativa de las fechas en una línea de tiempo, teniendo en cuenta los años bisiestos.

## Ver también

- Documentación oficial de Ruby sobre el método Date#<=>: https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html#method-i-3C-3D-3E
- Documentación oficial de Ruby sobre el método Date#eql?: https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html#method-i-eql-3F
- Documentación oficial de Ruby sobre el método Date#between?: https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html#method-i-between-3F