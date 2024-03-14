---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:53.694868-07:00
description: "JSON (Notaci\xF3n de Objetos de JavaScript) es un formato ligero de\
  \ intercambio de datos, prevalente en aplicaciones web para el intercambio de datos\
  \ entre\u2026"
lastmod: '2024-03-13T22:44:59.612347-06:00'
model: gpt-4-0125-preview
summary: "JSON (Notaci\xF3n de Objetos de JavaScript) es un formato ligero de intercambio\
  \ de datos, prevalente en aplicaciones web para el intercambio de datos entre\u2026"
title: Trabajando con JSON
---

{{< edit_this_page >}}

## Qué y Por Qué?

JSON (Notación de Objetos de JavaScript) es un formato ligero de intercambio de datos, prevalente en aplicaciones web para el intercambio de datos entre clientes y servidores. Los programadores trabajan con JSON en Ruby para analizar datos recibidos de fuentes externas o para formatear datos para respuestas de API, aprovechando su estructura legible por humanos para la manipulación y almacenamiento de datos fácilmente.

## Cómo:

Ruby, con su biblioteca estándar, proporciona formas sin fisuras para analizar y generar JSON. El módulo principal para estas operaciones es `json`, que se puede integrar fácilmente en cualquier aplicación Ruby.

### Analizando JSON:

Para convertir una cadena JSON en un hash de Ruby, puedes usar el método `JSON.parse`.

```ruby
require 'json'

json_string = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_string)

puts ruby_hash
# Salida: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### Generando JSON:

De manera inversa, para convertir un hash de Ruby en una cadena JSON, usas el método `JSON.generate` o el método `to_json` disponible en objetos Ruby una vez que se requiere la biblioteca `json`.

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_string = ruby_hash.to_json

puts json_string
# Salida: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### Bibliotecas de Terceros:

Si bien la biblioteca estándar de Ruby cubre el manejo básico de JSON, muchos proyectos dependen de bibliotecas de terceros para una funcionalidad y rendimiento mejorados. Una opción popular es `Oj` (Optimized JSON).

#### Analizando con Oj:

```ruby
require 'oj'

json_string = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_string)

puts ruby_hash
# Salida: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### Generando con Oj:

Oj también ofrece una forma rápida de generar JSON a partir de objetos Ruby:

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_string = Oj.dump(ruby_hash)

puts json_string
# Salida: {"name":"Samantha","age":35,"city":"Miami"}
```

Estos ejemplos ilustran la naturaleza sencilla de trabajar con JSON en Ruby, haciéndolo accesible para tareas que van desde manipulaciones de datos simples hasta comunicaciones complejas de API.
