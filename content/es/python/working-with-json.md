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

## ¿Por qué utilizar JSON en programación?

JSON (JavaScript Object Notation) es un formato de datos popular en programación debido a su simplicidad y universalidad. Almacenar y transmitir datos en formato JSON permite una fácil lectura y manipulación por parte de los humanos y las máquinas. Además, es compatible con una gran cantidad de lenguajes de programación, lo que lo hace ideal para sistemas interconectados.

## Cómo utilizar JSON en Python

Para trabajar con JSON en Python, primero debemos importar el módulo `json`:

```Python
import json
```

Podemos convertir un objeto de Python en formato JSON utilizando el método `dumps()`:

```Python
my_dict = {"name": "John", "age": 30}
json_data = json.dumps(my_dict)
print(json_data)
```
Output:
```Python
'{"name": "John", "age": 30}'
```

Para convertir una cadena de JSON en un objeto de Python, podemos utilizar el método `loads()`:

```Python
json_data = '{"name": "John", "age": 30}'
my_dict = json.loads(json_data)
print(my_dict)
```
Output:
```Python
{'name': 'John', 'age': 30}
```

También podemos trabajar con archivos JSON utilizando la función `load()` para cargar el contenido del archivo en un objeto de Python:

```Python
with open('data.json') as f:
    json_data = json.load(f)
print(json_data)
```

## Profundizando en el trabajo con JSON en Python

Aparte de la conversión entre objetos de Python y JSON, el módulo `json` también ofrece funciones como `dump()` para guardar datos en formato JSON en un archivo y `update()` para actualizar un objeto de Python con datos de un archivo JSON. También podemos especificar opciones de formato al guardar los datos, como indentación y orden de las claves.

En casos donde se requiere trabajar con datos más complejos, el módulo `json` también proporciona funciones para validar la estructura de los datos JSON, como `validate()` y `is_valid()`, así como opciones para personalizar la validación.

## Ver también

- Documentación oficial de Python sobre el módulo `json`: https://docs.python.org/es/3/library/json.html
- Ejemplos de uso de JSON en Python: https://www.programiz.com/python-programming/json