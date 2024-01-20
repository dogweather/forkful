---
title:                "Trabajando con JSON"
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/working-with-json.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

JSON (JavaScript Object Notation) es un formato ligero para el intercambio de datos. Los programadores lo usan por su facilidad de lectura para humanos y por ser sencillo de interpretar y generar para las máquinas.

## Cómo Hacerlo:

Para manipular JSON en Python, usamos el módulo `json`, que viene incorporado en el lenguaje. Aquí unos ejemplos básicos:

```Python
import json

# Convertir de JSON a un diccionario de Python (Deserialización)
json_string = '{"nombre": "Carmen", "edad": 30, "ciudad": "Madrid"}'
persona = json.loads(json_string)
print(persona)

# Convertir un diccionario de Python a JSON (Serialización)
persona_dict = {"nombre": "Carmen", "edad": 30, "ciudad": "Madrid"}
json_string = json.dumps(persona_dict, indent=4)
print(json_string)
```

Salida:

```
{'nombre': 'Carmen', 'edad': 30, 'ciudad': 'Madrid'}
{
    "nombre": "Carmen",
    "edad": 30,
    "ciudad": "Madrid"
}
```

## Profundizando:

El uso de JSON como formato de intercambio de datos se popularizó a principios de los años 2000 gracias a su simplicidad frente a XML. Alternativas modernas incluyen YAML, que es aún más legible para humanos, y Protocol Buffers, que es más eficiente para la comunicación entre máquinas. Mientras que JSON se decodifica en diccionarios y listas en Python, es importante manejar correctamente los tipos de datos soportados y las conversiones, teniendo en cuenta cosas como los números grandes y las fechas/tiempos.

## Ver También:

- Documentación oficial de Python para el módulo `json`: [link](https://docs.python.org/3/library/json.html)
- W3Schools para un tutorial de JSON en Python: [link](https://www.w3schools.com/python/python_json.asp)
- JSON en 10 minutos: una guía rápida para principiantes: [link](https://www.youtube.com/watch?v=iiADhChRriM)