---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:27.089663-07:00
description: "C\xF3mo hacerlo: La biblioteca est\xE1ndar de Ruby incluye las clases\
  \ `Date` y `Time` para manejar fechas y horas. As\xED es como se obtiene la fecha\
  \ actual."
lastmod: '2024-03-13T22:44:59.601629-06:00'
model: gpt-4-0125-preview
summary: "La biblioteca est\xE1ndar de Ruby incluye las clases `Date` y `Time` para\
  \ manejar fechas y horas."
title: Obteniendo la fecha actual
weight: 29
---

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
