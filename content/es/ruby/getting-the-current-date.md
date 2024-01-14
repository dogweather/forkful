---
title:                "Ruby: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Por qué

En programación, obtener la fecha y hora actual es una tarea común pero importante. Ya sea para registrar eventos en una aplicación, mostrar la fecha actual en una página web o simplemente para fines de depuración, tener acceso a la fecha y hora actual es esencial para muchas tareas. En este artículo, vamos a explorar cómo obtener fácilmente la fecha y hora actual en Ruby.

##Cómo hacerlo

Ruby tiene una clase incorporada llamada `Time` que nos permite obtener la fecha y hora actual. Echemos un vistazo a cómo podemos usar esta clase para obtener la fecha y hora actual.

```Ruby
# Obtener la fecha y hora actual
current_time = Time.now

# Obtener la fecha actual
current_date = current_time.to_date

# Obtener la hora actual
current_hour = current_time.hour

# Obtener la fecha y hora en un formato específico
formatted_time = current_time.strftime("%d/%m/%Y %H:%M:%S")

# Imprimir la fecha y hora actual
puts "La fecha y hora actual son: #{formatted_time}"
```

En el ejemplo de código anterior, primero creamos una variable `current_time` que almacena la fecha y hora actual utilizando el método `now` de la clase `Time`. Luego, utilizamos algunos métodos adicionales para obtener la fecha, la hora y un formato específico para la fecha y hora. Por último, imprimimos la fecha y hora actual en la consola.

##Profundizando

La clase `Time` también tiene muchos otros métodos útiles, como `year`, `month`, `day`, `minute`, `second`, entre otros, que nos permiten obtener valores específicos de la fecha y hora actual. También podemos usar estos métodos para realizar cálculos y manipulaciones de fechas.

Además, Ruby también tiene una gema (gem) llamada `Date` que proporciona una funcionalidad más avanzada para trabajar con fechas y horas. Esta gema nos permite crear objetos de fecha, comparar fechas y realizar otras operaciones importantes.

En resumen, aunque obtener la fecha y hora actual puede parecer una tarea sencilla, Ruby nos ofrece varias opciones y herramientas para manejar fechas y horas de manera más eficiente y precisa.

##Ver también

- [Documentación de la clase Time en Ruby] (https://ruby-doc.org/core-3.0.2/Time.html)
- [Documentación de la gema Date en Ruby] (https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/Date.html)