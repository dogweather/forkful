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

## ¿Qué y Por qué?
Comparar dos fechas es una tarea común en la programación, que implica comparar dos objetos de tipo fecha para determinar cuál es la anterior o si son iguales. Esto es importante en muchas aplicaciones, como calendarios, programas de reservas o sistemas de seguimiento de tiempo. ¡Los programadores lo hacen todo el tiempo!

## Cómo hacerlo: 

Utilizar Ruby para comparar dos fechas es muy sencillo. Simplemente utiliza el operador `>` (mayor que) o `<` (menor que) entre dos objetos de tipo fecha. Por ejemplo:

```Ruby
time1 = Time.new(2021, 05, 21)
time2 = Time.new(2021, 04, 15)
puts time1 > time2
# Output: true
```

## Profundizando

La comparación de fechas ha sido una tarea importante en la informática ya desde los primeros días de la programación. Diferentes lenguajes de programación tienen diferentes métodos de comparación de fechas, pero en Ruby, podemos simplemente utilizar los operadores `>` y `<`.

Sin embargo, si necesitas una comparación más precisa, puedes usar la biblioteca `Date` de Ruby, que ofrece métodos específicos para comparar fechas con mayor precisión. Otro enfoque es convertir las fechas a un número de días y luego compararlos. 

## Ver también:

 - [La documentación oficial de Ruby sobre comparación de fechas](https://ruby-doc.org/core-3.0.1/Date.html)
 - [Artículo de Medium sobre comparación de fechas en Ruby](https://medium.com/@thegimanator/how-to-compare-dates-in-ruby-ad9d07089a01)
 - [Tutorial de Ruby sobre uso de la clase `Date`](https://www.rubyguides.com/2015/10/ruby-date-time/)