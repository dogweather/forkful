---
title:                "Trabajando con YAML"
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Trabajar con YAML significa manejar un formato de serialización de datos muy leído por humanos, usado para configuraciones y transmisión de datos. Los programadores lo usan por su simplicidad y facilidad de lectura comparado con XML o JSON.

## Cómo Hacerlo:
Gleam aún no tiene una biblioteca estándar para YAML, así que hipotéticamente podríamos usar una librería externa o ejecutar un binario de CLI con `shell` (si existiera). Aquí tienes un snippet que mostraría cómo sería trabajar con YAML suponiendo que existiera una librería:

```gleam
import hypothetical_yaml_lib.{parse_yaml, emit_yaml}

// Parsear YAML a una estructura Gleam
let data: Result(Yaml, Error) = parse_yaml("""
- name: John Doe
  age: 30
- name: Jane Smith
  age: 25
""")

// Convertir un mapa a YAML
let yaml_string: Result(String, Error) = emit_yaml(data)
```

Suponiendo que esto funcione, tendrías algo como:

```plaintext
- name: John Doe
  age: 30
- name: Jane Smith
  age: 25
```

## Inmersión Profunda:
YAML inició en 2001 como "Yet Another Markup Language", pero se remarcó como "YAML Ain't Markup Language" para enfatizar su naturaleza de formato de datos. En cuanto a alternativas, JSON y XML son bien conocidos, pero mientras JSON es similar en legibilidad, XML es más verbose y complejo. Al implementarlo con Gleam, considera los riesgos de inyección de YAML si se maneja entrada del usuario, dado que YAML carga en tipos nativos.

## Ver También:
- Especificaciones de YAML: [yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- Tutorial de YAML: [learnxinyminutes.com/docs/yaml](https://learnxinyminutes.com/docs/yaml/)