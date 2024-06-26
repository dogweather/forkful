---
date: 2024-01-20 17:31:58.137512-07:00
description: "C\xF3mo hacerlo: Ruby hace que jugar con fechas sea pan comido. Usa\
  \ la clase `Date` para manejos b\xE1sicos o `DateTime` si necesitas m\xE1s precisi\xF3\
  n con horas,\u2026"
lastmod: '2024-03-13T22:44:59.604388-06:00'
model: gpt-4-1106-preview
summary: Ruby hace que jugar con fechas sea pan comido.
title: Calcular una fecha en el futuro o pasado
weight: 26
---

## Cómo hacerlo:
Ruby hace que jugar con fechas sea pan comido. Usa la clase `Date` para manejos básicos o `DateTime` si necesitas más precisión con horas, minutos y segundos. Aquí te muestro cómo hacerlo:

```Ruby
require 'date'

# Sumar días a una fecha
hoy = Date.today
futuro = hoy + 10
puts futuro  # => YYYY-MM-DD

# Restar días a una fecha
pasado = hoy - 10
puts pasado  # => YYYY-MM-DD

# Para DateTime, también puedes sumar horas y minutos
ahora = DateTime.now
futuro_detallado = ahora + (60 * 60 * 24)  # Añade un día (en segundos)
puts futuro_detallado  # => YYYY-MM-DDTHH:MM:SS+ZZ:ZZ
```

Y así, puedes calcular fechas hacia adelante o atrás tan fácil como un paseo por el parque.

## Profundizando:
Antes, calcular fechas era una tarea peliaguda que requería tablas y almanaques. Con la evolución de los lenguajes de programación, esto se ha simplificado enormemente. Ruby, en particular, ofrece una biblioteca estándar (`date`) que maneja muchas de las complejidades por nosotros.

Alternativas: Si `Date` y `DateTime` no cumplen con tus expectativas, siempre puedes usar gems como `ActiveSupport::TimeWithZone` si estás en un contexto de Rails, que añade métodos como `days.from_now` y `days.ago`.

Los detalles de implementación sobre las fechas en Ruby son importantes. `Date` y `DateTime` manejan años bisiestos, diferentes zonas horarias y hasta cambios históricos del calendario (como la adopción del calendario Gregoriano).

## Ver También:
- La documentación oficial de Ruby para [Date](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html) y [DateTime](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/DateTime.html)
- [`ActiveSupport::TimeWithZone`](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html) si trabajas con Rails
- Una biblioteca robusta para manipulación de fechas/tiempos: [Chronic](https://github.com/mojombo/chronic)
