---
title:                "Trabajando con YAML"
aliases: - /es/ruby/working-with-yaml.md
date:                  2024-02-03T19:26:16.561306-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
YAML, que significa "YAML Ain't Markup Language" (YAML No Es Un Lenguaje de Marcado), es ampliamente usado en Ruby para archivos de configuración y serialización de datos debido a su formato legible por humanos. Los programadores se inclinan hacia YAML cuando necesitan almacenar o transmitir objetos de datos de manera legible pero estructurada, simplificando tareas como la gestión de configuración, almacenamiento de datos e intercambio de datos entre lenguajes.

## Cómo hacerlo:
Ruby viene con una biblioteca incorporada llamada Psych para analizar y emitir YAML. Para utilizarla, primero necesitas requerir la biblioteca estándar de YAML. Aquí tienes un ejemplo básico para comenzar:

```ruby
require 'yaml'

# Hash a ser serializado
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# Convirtiendo el hash a YAML
yaml_data = person.to_yaml

puts yaml_data
```

**Salida de muestra:**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

Para cargar datos YAML de vuelta en un objeto de Ruby:

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**Salida de muestra:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### Usando Bibliotecas de Terceros:

Aunque la biblioteca estándar es suficiente para tareas básicas, para necesidades complejas podrías considerar bibliotecas de terceros como 'safe_yaml'. Para usar tales bibliotecas, primero debes instalar la gema:

```bash
gem install safe_yaml
```

Luego, puedes usarla para cargar datos YAML de forma segura, mitigando riesgos como la instanciación de objetos desde fuentes controladas por el usuario:

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**Salida de muestra:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

Este enfoque mejora la seguridad de tu manejo de YAML, convirtiéndolo en una buena opción para aplicaciones que cargan YAML de fuentes no confiables.
