---
title:                "Obteniendo la fecha actual"
date:                  2024-02-03T19:10:27.089663-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obteniendo la fecha actual"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Obtener la fecha actual es una tarea esencial en casi cualquier esfuerzo de programación, desde registrar actividades en una aplicación hasta generar informes con sellos de fecha. En Ruby, esto se puede lograr fácilmente utilizando la biblioteca estándar, simplificando operaciones que involucran fechas.

## Cómo hacerlo:
La biblioteca estándar de Ruby incluye las clases `Date` y `Time` para manejar fechas y horas. Así es como se obtiene la fecha actual:

```ruby
require 'date'

current_date = Date.today
puts current_date
```

Salida de muestra:
```
2023-04-12
```

Para incluir la hora con la fecha, la clase `Time` de Ruby es más adecuada:

```ruby
current_time = Time.now
puts current_time
```

Salida de muestra:
```
2023-04-12 14:33:07 +0200
```

Si necesitas más funcionalidad, como la gestión de zonas horarias, podrías querer usar una gema de terceros como `ActiveSupport` (parte de Rails pero se puede usar de manera independiente).

Primero, agrega `activesupport` a tu Gemfile y ejecuta `bundle install`:

```ruby
gem 'activesupport'
```

Luego, úsalo para manejar zonas horarias:

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # Establece tu zona horaria deseada
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

Salida de muestra:
```
Wed, 12 Apr 2023 08:33:07 EDT -04:00
```
