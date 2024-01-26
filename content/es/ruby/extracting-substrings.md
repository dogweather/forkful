---
title:                "Extracción de subcadenas"
date:                  2024-01-20T17:46:29.967487-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracción de subcadenas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Extraer substrings significa sacar partes específicas de un string. Lo hacemos para manipular, analizar o modificar datos de manera más precisa.

## How to:

```Ruby
# Strings son colecciones de caracteres
frase = "Hola, bienvenido a Ruby!"

# Extraer substring usando índices [inicio, longitud]
saludo = frase[0, 4] # => "Hola"

# Extracción con rango de índices
bienvenida = frase[6, 11] # => "bienvenido"

# Ruby también acepta rangos con ..
lenguaje = frase[17..20] # => "Ruby"

puts saludo
puts bienvenida
puts lenguaje
```

Salida:
```
Hola
bienvenido
Ruby
```

## Profundizando

Extracting substrings es esencial desde los inicios del lenguaje por su utilidad en campos como el procesamiento de texto, parsing y en el desarrollo web. En versiones antiguas de Ruby, métodos como `slice` o sus alias se usaban frecuentemente, y aunque todavía son válidos, la sintaxis de índices con corchetes ha ganado popularidad por su claridad.

En cuanto a alternativas, se puede usar la gema `strscan` para extracciones más complejas o la función `split` para dividir strings en arrays por algún delimitador, y luego extraer los substrings necesarios.

Un detalle de implementación a considerar es que extraer substrings crea nuevos objetos string en memoria, lo que es importante en términos de rendimiento si se trabaja con strings muy grandes o en operaciones a gran escala.

## Ver También

- Documentación oficial de Ruby para strings: [Ruby-Doc String](https://ruby-doc.org/core/String.html)
