---
title:                "Trabajando con json"
html_title:           "Python: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/working-with-json.md"
---

{{< edit_this_page >}}

# Qué es y por qué lo hacemos?

Trabajar con JSON (JavaScript Object Notation) en Python es una forma sencilla y eficiente de almacenar y manejar datos estructurados. Los programadores utilizan JSON porque es fácil de leer y escribir, y es un formato comúnmente utilizado para intercambiar datos en aplicaciones web.

# Cómo hacerlo:

Python tiene una librería incorporada para trabajar con JSON, la cual incluye una función para convertir objetos Python en JSON, y otra para convertir JSON en objetos Python.

```Python
# Convertir objeto Python en JSON
import json
my_dict = {"nombre": "Juan", "edad": 30, "ciudad": "Madrid"}
json_data = json.dumps(my_dict)

# Convertir JSON en objeto Python
import json
json_data = '{"nombre": "Juan", "edad": 30, "ciudad": "Madrid"}'
my_dict = json.loads(json_data)
```

# Profundizando:

Antes de JSON, los programadores solían utilizar formatos de texto como XML o CSV para intercambiar datos estructurados. Sin embargo, JSON es más ligero y fácil de leer y escribir, lo que lo hace ideal para aplicaciones web.

Además de la librería estándar de Python, también existen librerías de terceros que ofrecen características adicionales para trabajar con JSON, como la validación de esquemas o el manejo de grandes volúmenes de datos.

# Referencias:

- Documentación oficial de Python para trabajar con JSON: https://docs.python.org/es/3/library/json.html
- Paquete `jsonschema` para validar esquemas de JSON: https://pypi.org/project/jsonschema/
- Paquete `ujson` para un rendimiento más rápido en la manipulación de datos JSON: https://pypi.org/project/ujson/