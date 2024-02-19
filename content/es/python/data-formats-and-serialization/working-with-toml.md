---
aliases:
- /es/python/working-with-toml/
date: 2024-01-26 04:25:13.050505-07:00
description: "TOML, abreviatura de Tom's Obvious, Minimal Language, es un formato\
  \ de serializaci\xF3n de datos parecido a JSON o YAML, pero apunta a la simplicidad\
  \ y\u2026"
lastmod: 2024-02-18 23:09:09.583013
model: gpt-4-0125-preview
summary: "TOML, abreviatura de Tom's Obvious, Minimal Language, es un formato de serializaci\xF3\
  n de datos parecido a JSON o YAML, pero apunta a la simplicidad y\u2026"
title: Trabajando con TOML
---

{{< edit_this_page >}}

## Qué y Por Qué?
TOML, abreviatura de Tom's Obvious, Minimal Language, es un formato de serialización de datos parecido a JSON o YAML, pero apunta a la simplicidad y legibilidad. Los programadores utilizan TOML para archivos de configuración porque es fácil de escribir y entender, y se mapea de manera ordenada a estructuras de datos en lenguajes de programación como Python.

## Cómo:
Antes de sumergirnos, instala el paquete `toml` con `pip install toml`. Vamos a analizar un archivo TOML:

```python
import toml

# Ejemplo de contenido TOML como una cadena
toml_string = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # Fechas de primera clase

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# Analizar la cadena TOML
parsed_toml = toml.loads(toml_string)

# Acceder a los datos
print(parsed_toml['owner']['name'])  # Salida: Tom Preston-Werner
print(parsed_toml['database']['ports'])  # Salida: [8001, 8001, 8002]
```

## Profundización
TOML fue creado por Tom Preston-Werner, uno de los fundadores de GitHub, como un formato de archivo de configuración más amigable para el usuario. Está diseñado para mapear de manera no ambigua a una tabla hash y ser fácilmente analizable por máquinas.

Comparado con JSON, TOML es más legible para archivos de configuración y admite comentarios. YAML, otra alternativa, puede ser más compacto, pero su dependencia de la indentación y problemas sutiles, como la prohibición de tabulaciones, pueden confundir a las personas.

En cuanto a los detalles de implementación, los valores TOML están tipados, lo que incluye cadenas, enteros, flotantes, booleanos, fechas y horas, arreglos y tablas. Todo es sensible a mayúsculas y minúsculas. Además, TOML admite cadenas de múltiples líneas y, a partir de la última versión, incluso permite arreglos de tipos heterogéneos.

Python utiliza la biblioteca `toml`, la cual se asemeja a las bibliotecas JSON y YAML en términos de API. Tienes `toml.load` y `toml.loads` para leer TOML de un archivo o una cadena, respectivamente, y `toml.dump` y `toml.dumps` para escribirlo.

## Ver También
- El repositorio oficial de TOML en GitHub para especificaciones: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- La documentación de la biblioteca `toml` de Python: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- Ejemplos del mundo real de TOML: Archivos de configuración para el administrador de paquetes de Rust `cargo` o la herramienta de empaquetado de Python `poetry`.
