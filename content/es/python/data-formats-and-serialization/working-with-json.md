---
aliases:
- /es/python/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:00.539955-07:00
description: "Trabajar con JSON (Notaci\xF3n de Objetos de JavaScript) implica parsear\
  \ cadenas formateadas en JSON a objetos de Python y viceversa. Esto es crucial para\
  \ el\u2026"
lastmod: 2024-02-18 23:09:09.579752
model: gpt-4-0125-preview
summary: "Trabajar con JSON (Notaci\xF3n de Objetos de JavaScript) implica parsear\
  \ cadenas formateadas en JSON a objetos de Python y viceversa. Esto es crucial para\
  \ el\u2026"
title: Trabajando con JSON
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Trabajar con JSON (Notación de Objetos de JavaScript) implica parsear cadenas formateadas en JSON a objetos de Python y viceversa. Esto es crucial para el desarrollo de web y API ya que JSON es la lingua franca para el intercambio de datos entre servidores y clientes.

## Cómo hacerlo:

La biblioteca integrada `json` de Python simplifica el proceso de codificación (convertir objetos de Python a JSON) y decodificación (convertir JSON a objetos de Python). Así es como puedes usarla:

### Codificando objetos Python a JSON:

```python
import json

datos = {
    "nombre": "John Doe",
    "edad": 30,
    "esEmpleado": True,
    "direcciones": [
        {"ciudad": "Nueva York", "codigoPostal": "10001"},
        {"ciudad": "San Francisco", "codigoPostal": "94016"}
    ]
}

cadena_json = json.dumps(datos, indent=4)
print(cadena_json)
```

**Salida:**

```json
{
    "nombre": "John Doe",
    "edad": 30,
    "esEmpleado": true,
    "direcciones": [
        {
            "ciudad": "Nueva York",
            "codigoPostal": "10001"
        },
        {
            "ciudad": "San Francisco",
            "codigoPostal": "94016"
        }
    ]
}
```

### Decodificando JSON a objetos Python:

```python
cadena_json = '''
{
    "nombre": "John Doe",
    "edad": 30,
    "esEmpleado": true,
    "direcciones": [
        {
            "ciudad": "Nueva York",
            "codigoPostal": "10001"
        },
        {
            "ciudad": "San Francisco",
            "codigoPostal": "94016"
        }
    ]
}
'''

datos = json.loads(cadena_json)
print(datos)
```

**Salida:**

```python
{
    'nombre': 'John Doe', 
    'edad': 30, 
    'esEmpleado': True, 
    'direcciones': [
        {'ciudad': 'Nueva York', 'codigoPostal': '10001'}, 
        {'ciudad': 'San Francisco', 'codigoPostal': '94016'}
    ]
}
```

### Trabajando con bibliotecas de terceros:

Para manejar JSON complejo, como la validación de esquemas o el análisis de archivos JSON directamente desde URLs, bibliotecas como `requests` para solicitudes HTTP y `jsonschema` para validación pueden ser útiles.

#### Ejemplo con `requests` para analizar JSON desde una URL:

```python
import requests

respuesta = requests.get('https://api.ejemplo.com/datos')
datos = respuesta.json()

print(datos)
```

Este fragmento obtiene datos JSON de una URL dada y los convierte directamente en un objeto Python.

#### Usando `jsonschema` para validar JSON:

Primero, instala la biblioteca mediante pip:

```bash
pip install jsonschema
```

Luego, úsala de la siguiente manera:

```python
from jsonschema import validate
import jsonschema

esquema = {
    "tipo": "objeto",
    "propiedades": {
        "nombre": {"tipo": "cadena"},
        "edad": {"tipo": "número"},
        "esEmpleado": {"tipo": "booleano"},
    },
    "requerido": ["nombre", "edad", "esEmpleado"]
}

# Suponiendo que `datos` es un diccionario obtenido de la decodificación de JSON
try:
    validate(instance=datos, schema=esquema)
    print("Datos JSON válidos.")
except jsonschema.exceptions.ValidationError as err:
    print("Error de validación:", err)
```

Este ejemplo valida tu diccionario Python (obtenido de datos JSON decodificados) contra un esquema predefinido, asegurando que los datos se ajusten a los formatos y tipos esperados.
