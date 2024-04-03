---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:08.429086-07:00
description: "C\xF3mo hacerlo: En Ruby, la biblioteca est\xE1ndar proporciona maneras\
  \ directas de interpretar fechas de strings usando las clases `Date` y `DateTime`.\
  \ As\xED es\u2026"
lastmod: '2024-03-13T22:44:59.600595-06:00'
model: gpt-4-0125-preview
summary: "En Ruby, la biblioteca est\xE1ndar proporciona maneras directas de interpretar\
  \ fechas de strings usando las clases `Date` y `DateTime`."
title: Analizando una fecha a partir de una cadena de texto
weight: 30
---

## Cómo hacerlo:
En Ruby, la biblioteca estándar proporciona maneras directas de interpretar fechas de strings usando las clases `Date` y `DateTime`. Así es como lo haces usando los métodos integrados de Ruby:

```ruby
require 'date'

# Interpretar una fecha de un string
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# DateTime para una representación detallada del tiempo
datetime_string = "2023-04-01T15:30:45+00:00"
parsed_datetime = DateTime.parse(datetime_string)
puts parsed_datetime
# => 2023-04-01T15:30:45+00:00
```

Para tener más control o manejar formatos que `parse` podría no entender directamente, puedes usar `strptime` (analizar tiempo en string), especificando el formato explícitamente:

```ruby
# Usando strptime para formatos personalizados
custom_date_string = "01-04-2023"
parsed_date_custom = Date.strptime(custom_date_string, '%d-%m-%Y')
puts parsed_date_custom
# => 2023-04-01
```

### Usando bibliotecas de terceros:
Aunque las capacidades incorporadas de Ruby son poderosas, a veces podrías preferir bibliotecas de terceros por funciones adicionales o una sintaxis más simple. Una elección popular es la gema `Chronic` para la interpretación de lenguaje natural:

1. Primero, agrega Chronic a tu Gemfile y ejecuta `bundle install`:
```ruby
gem 'chronic'
```

2. Luego, úsalo de esta manera:
```ruby
require 'chronic'

parsed_chronic = Chronic.parse('next Tuesday')
puts parsed_chronic
# La salida variará dependiendo de la fecha actual; asumiendo que se interpreta el 2023-04-01
# => 2023-04-04 12:00:00 +0000
```

`Chronic` es muy útil para la entrada de usuario ya que puede entender una amplia gama de formatos de fecha en lenguaje natural, lo que lo hace una herramienta poderosa para aplicaciones que requieren una entrada de fecha flexible.
