---
title:                "Calculando una fecha en el futuro o en el pasado"
html_title:           "Ruby: Calculando una fecha en el futuro o en el pasado"
simple_title:         "Calculando una fecha en el futuro o en el pasado"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# ¡Calculando fechas en el futuro o pasado con Ruby!

## ¿Qué y por qué?

Calcular una fecha en el futuro o pasado es una tarea común en la programación, especialmente cuando se trabaja con fechas y horarios en aplicaciones y sistemas.

Los programadores utilizan esta función para realizar cálculos con fechas y horarios, como por ejemplo, mostrar eventos próximos o calcular la duración entre dos fechas.

## ¡Cómo hacerlo!

Para calcular una fecha en el futuro o pasado utilizando Ruby, primero necesitas tener una fecha inicial y un número de días o segundos a agregar o restar. Luego, puedes utilizar los siguientes métodos:

```Ruby
# Sumar días a una fecha
fecha_inicio + numero_de_dias

# Restar días a una fecha
fecha_inicio - numero_de_dias

# Sumar segundos a una fecha
fecha_inicio + numero_de_segundos

# Restar segundos a una fecha
fecha_inicio - numero_de_segundos
```

¡Así de sencillo! Veamos un ejemplo en acción:

```Ruby
# Definimos una fecha inicial
fecha_inicio = Time.new(2021, 9, 1)

# Sumamos 10 días a la fecha inicial
fecha_fin = fecha_inicio + 10

puts fecha_fin
# Output: 2021-09-11 00:00:00 +0200
```

En este ejemplo, hemos creado una variable que contiene una fecha inicial y luego utilizamos el operador de suma para agregar 10 días a esa fecha. Después, imprimimos el resultado que es una nueva fecha con el formato especificado.

## ¡Profundizando más!

### Contexto histórico

La manipulación de fechas y horarios es un tema que ha evolucionado a lo largo de los años en la programación. Antes, los programadores tenían que trabajar con números enteros para representar las fechas, mientras que ahora contamos con herramientas más avanzadas como la librería de fechas de Ruby.

### Alternativas

Además de utilizar los operadores de suma y resta, también existen otras formas de calcular fechas en el futuro o pasado en Ruby. Por ejemplo, puedes utilizar el método `advance` de la clase `Date` o `DateTime` para agregar o restar diferentes unidades de tiempo (días, meses, años) a una fecha dada.

### Detalles de implementación

Internamente, Ruby utiliza el conteo de segundos desde la medianoche del 1 de enero de 1970 (sistema Unix) para representar las fechas y horarios. Esto facilita la realización de cálculos en diferentes zonas horarias ya que la hora y fecha inicial siempre se basan en un mismo punto de referencia.

## ¡Links relacionados!

- [Documentación de fechas en Ruby](https://ruby-doc.org/core-3.0.2/Time.html)
- [Método advance en la clase Date](https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/Date.html#method-i-advance)
- [Método advance en la clase DateTime](https://ruby-doc.org/stdlib-3.0.2/libdoc/datetime/rdoc/DateTime.html#method-i-advance)