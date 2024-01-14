---
title:                "Python: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/working-with-json.md"
---

{{< edit_this_page >}}

## Por qué trabajar con JSON en Python

Si estás buscando una manera sencilla y flexible de almacenar y transmitir datos en tus proyectos de programación en Python, entonces JSON es una excelente opción.

## Cómo hacerlo

Para trabajar con JSON en Python, primero necesitas importar la librería **json**, la cual viene incluida en la instalación estándar de Python. Una vez que hayas importado la librería, puedes comenzar a utilizarla para convertir objetos de Python en formato JSON y viceversa.

Aquí hay un ejemplo de cómo crear un diccionario en Python y luego convertirlo en un formato JSON usando la función `dumps`:

```python
import json

# Crear un diccionario
mi_diccionario = {'nombre': 'Juan', 'edad': 28, 'ocupacion': 'programador'}

# Convertir a JSON
mi_json = json.dumps(mi_diccionario)

# Imprimir resultados
print(type(mi_json)) # <class 'str'>
print(mi_json) # '{"nombre": "Juan", "edad": 28, "ocupacion": "programador"}'
```

Podemos ver que el diccionario ha sido convertido en una cadena de texto en formato JSON. Ahora, para convertir un objeto JSON en un diccionario de Python, podemos utilizar la función `loads`:

```python
# Convertir de JSON a diccionario
otro_diccionario = json.loads(mi_json)

# Imprimir resultados
print(type(otro_diccionario)) # <class 'dict'>
print(otro_diccionario) # {'nombre': 'Juan', 'edad': 28, 'ocupacion': 'programador'}
```

Como podemos ver, el objeto JSON ha sido convertido de vuelta a un diccionario de Python.

## Profundizando

JSON, acrónimo de *JavaScript Object Notation*, es un formato de intercambio de datos ligero y fácil de entender. Fue originalmente diseñado para ser utilizado junto con JavaScript, pero hoy en día es ampliamente utilizado en muchos lenguajes de programación, incluyendo Python.

Una de las mayores ventajas de trabajar con JSON en Python es que es compatible con muchos tipos de datos, incluyendo diccionarios, listas, cadenas, números y booleanos. Esto lo hace muy flexible y útil para una amplia gama de aplicaciones.

Además, JSON es un formato de texto, lo que significa que es fácil de leer y escribir para los humanos y también fácil de analizar y generar para los computadores. Esto lo hace ideal para el intercambio de datos en aplicaciones web y servicios web.

En resumen, trabajar con JSON en Python te permite intercambiar datos de manera sencilla y eficiente en tus proyectos de programación.

## Ver también

- [Documentación de la librería JSON de Python](https://docs.python.org/es/3/library/json.html)
- [Tutorial de JSON en Python](https://www.programiz.com/python-programming/json)
- [Introducción a JSON en Python](https://realpython.com/python-json/)