---
aliases:
- /es/ruby/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:08.429086-07:00
description: "Interpretar una fecha de un string se trata de convertir texto que representa\
  \ una fecha en un objeto `Date` o `DateTime` que Ruby entiende. Los\u2026"
lastmod: 2024-02-18 23:09:10.562621
model: gpt-4-0125-preview
summary: "Interpretar una fecha de un string se trata de convertir texto que representa\
  \ una fecha en un objeto `Date` o `DateTime` que Ruby entiende. Los\u2026"
title: Analizando una fecha a partir de una cadena de texto
---

{{< edit_this_page >}}

## Qué y Por Qué?
Interpretar una fecha de un string se trata de convertir texto que representa una fecha en un objeto `Date` o `DateTime` que Ruby entiende. Los programadores hacen esto para realizar operaciones como comparaciones, cálculos o formatos en fechas, que son tareas comunes en aplicaciones que manejan programación de eventos, análisis o procesamiento de datos.

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
