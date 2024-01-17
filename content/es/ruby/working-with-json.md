---
title:                "Trabajando con json"
html_title:           "Ruby: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Qué y por qué?

Trabajar con JSON en Ruby es una forma de manejar datos estructurados en formato de texto. Los programadores utilizan JSON porque es un formato sencillo y fácil de leer, y es ampliamente utilizado en aplicaciones web y móviles.

## Cómo:

```Ruby
require 'json'

# Crear un objeto JSON
json_objeto = '{"nombre": "Juan", "edad": 25}'

# Convertir a hash
hash = JSON.parse(json_objeto)

# Acceder a los datos
nombre = hash["nombre"]
edad = hash["edad"]

# Convertir hash a JSON
json_hash = hash.to_json

# Imprimir resultados
puts nombre
puts edad
puts json_hash
```

Salida:

Juan
25
{"nombre":"Juan", "edad":25}

## Profundizando

JSON (JavaScript Object Notation) es un formato ligero y fácil de leer que originalmente se creó para JavaScript, pero ahora es ampliamente utilizado en la comunicación entre aplicaciones web y servidores web. Alternativas a JSON incluyen XML, YAML y CSV. En Ruby, se puede trabajar con JSON utilizando la librería estándar JSON o la gem 'oj'.

## Ver también

- [Página oficial de JSON](https://www.json.org/)
- [Documentación de la librería de Ruby JSON](https://ruby-doc.org/stdlib-2.6.5/libdoc/json/rdoc/JSON.html)
- [Gem 'oj' para trabajar con JSON en Ruby](https://rubygems.org/gems/oj)