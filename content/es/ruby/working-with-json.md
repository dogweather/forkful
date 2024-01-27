---
title:                "Trabajando con JSON"
date:                  2024-01-19
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
JSON (JavaScript Object Notation) es un formato estándar para intercambiar datos. Programadores lo usan por su simplicidad y facilidad de integración con distintos lenguajes, incluido Ruby.

## Cómo hacerlo:
Para trabajar con JSON en Ruby, primero necesitas tener la gema 'json' instalada. Puedes agregarla a tu `Gemfile` o instalarla ejecutando `gem install json`. Una vez instalada, parsear un JSON o convertir un objeto Ruby a JSON es sencillo:
```Ruby
require 'json'

# Convertir un string JSON a un hash de Ruby
json_string = '{"nombre": "Juan", "edad": 30, "programador": true}'
ruby_hash = JSON.parse(json_string)
p ruby_hash # => {"nombre"=>"Juan", "edad"=>30, "programador"=>true}

# Convertir un hash de Ruby a un string JSON
ruby_hash = { nombre: "Juan", edad: 30, programador: true }
json_string = ruby_hash.to_json
puts json_string # => {"nombre":"Juan","edad":30,"programador":true}
```

## Análisis Profundo:
JSON se originó en el 2001, diseñado por Douglas Crockford. Aunque vinculado inicialmente a JavaScript, su sencillez lo convirtió en un formato de intercambio de datos universal. Alternativas a JSON incluyen XML y YAML, pero JSON predomina por su ligereza y rápida interpretación. En Ruby, la gema 'json' viene incluida desde la versión 1.9, permitiendo su implementación sin dependencias externas. Para manejo avanzado de JSON, como métodos de parseo personalizados, se pueden usar gemas como `oj` (Optimized JSON).

## Ver También:
- Gem `oj` (Optimized JSON): [github.com/ohler55/oj](https://github.com/ohler55/oj)
- Tutorial completo de JSON en W3Schools: [w3schools.com/js/js_json_intro.asp](https://www.w3schools.com/js/js_json_intro.asp) (aunque enfocado en JavaScript, los conceptos básicos son aplicables a Ruby)
