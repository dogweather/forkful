---
title:                "Trabajando con YAML"
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?
YAML es un formato de serialización de datos legible por humanos, usado comúnmente para archivos de configuración y data intercambio entre lenguajes. Programadores lo usan por su simplicidad y legibilidad comparado con JSON o XML.

## Cómo hacerlo:
Ruby hace fácil trabajar con YAML usando la librería 'yaml'. Primero, instala la librería si aún no está:

```Ruby
gem install 'yaml'
```

Para parsear una cadena YAML:

```Ruby
require 'yaml'

# Una cadena YAML simple
yaml_string = <<-YAML
nombre: Juan
profesion: Desarrollador
YAML

# Parseando la cadena YAML
data = YAML.load(yaml_string)
puts data['nombre'] # Salida: Juan
```

Para generar una cadena YAML a partir de un hash de Ruby:

```Ruby
require 'yaml'

# Un hash de Ruby
ruby_hash = { nombre: 'Juan', profesion: 'Desarrollador' }

# Convirtiendo el hash a cadena YAML
yaml_output = ruby_hash.to_yaml
puts yaml_output
```

## Exploración Profunda
YAML, que significa "YAML Ain't Markup Language" (YAML no es un lenguaje de marcado), se diseñó para ser más fácil de comprender para los humanos que otros formatos de serialización de datos. A pesar de que JSON se ha vuelto más popular en APIs web, YAML es aún muy usado en herramientas de DevOps como Docker y Kubernetes. Además de 'yaml', existen otras librerías como 'safe_yaml' que enfocan en seguridad, minimizando los riesgos al parsear YAML.

## Ver También:
- Documentación oficial de Ruby para YAML: https://ruby-doc.org/stdlib-2.6.1/libdoc/yaml/rdoc/YAML.html
- YAML Specification: https://yaml.org/spec/1.2/spec.html
- Artículo "Working with YAML in Ruby": https://www.digitalocean.com/community/tutorials/how-to-work-with-yaml-in-ruby
- SafeYAML: https://github.com/dtao/safe_yaml
