---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:18.473156-07:00
description: "Los arreglos asociativos, conocidos en Python como diccionarios, mapean\
  \ claves a valores, facilitando la recuperaci\xF3n, modificaci\xF3n o seguimiento\
  \ de datos\u2026"
lastmod: '2024-03-13T22:44:58.603487-06:00'
model: gpt-4-0125-preview
summary: "Los arreglos asociativos, conocidos en Python como diccionarios, mapean\
  \ claves a valores, facilitando la recuperaci\xF3n, modificaci\xF3n o seguimiento\
  \ de datos\u2026"
title: Uso de matrices asociativas
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Los arreglos asociativos, conocidos en Python como diccionarios, mapean claves a valores, facilitando la recuperación, modificación o seguimiento de datos por un identificador único. Los programadores los utilizan por su eficiencia en acceder a elementos y su flexibilidad para representar estructuras de datos complejas.

## Cómo hacerlo:

Crear un diccionario en Python es sencillo. Encierras los pares clave-valor entre llaves `{}`, con las claves y los valores separados por dos puntos:

```Python
# Crear un arreglo asociativo (diccionario)
my_dict = {"name": "John", "age": 30, "city": "New York"}
print(my_dict)
```

Salida:
```
{'name': 'John', 'age': 30, 'city': 'New York'}
```

Acceder a un valor por su clave es simple:

```Python
# Acceder a un valor
print(my_dict["name"])
```

Salida:
```
John
```

Agregar o actualizar elementos se realiza asignando un valor a una clave:

```Python
# Agregar un nuevo par clave-valor
my_dict["email"] = "john@example.com"
# Actualizar un valor
my_dict["age"] = 31
print(my_dict)
```

Salida:
```
{'name': 'John', 'age': 31, 'city': 'New York', 'email': 'john@example.com'}
```

Para iterar sobre los elementos del diccionario:

```Python
# Iterar a través de los pares clave-valor
for key, value in my_dict.items():
    print(f"{key}: {value}")
```

Salida:
```
name: John
age: 31
city: New York
email: john@example.com
```

## Análisis Profundo

Los arreglos asociativos en Python, o diccionarios, se introdujeron para proporcionar una estructura de datos para el acceso y manipulación eficiente de datos. A diferencia de las secuencias, que están indexadas por un rango de números, los diccionarios están indexados por claves, que pueden ser de cualquier tipo inmutable. Esta elección de diseño hace que los diccionarios sean idealmente adecuados para tablas de búsqueda rápida donde las claves se mapean a valores únicos.

Históricamente, los diccionarios de Python se han implementado utilizando una tabla hash, asegurando que el tiempo promedio de complejidad para las operaciones de búsqueda, inserción y eliminación sea O(1). En Python 3.6 y posteriores, los diccionarios también mantienen el orden de inserción de los elementos, combinando los beneficios de las tablas hash con la previsibilidad del orden de inserción visto en las estructuras de datos ordenadas.

Aunque los diccionarios son increíblemente versátiles, en algunos casos especializados, alternativas como `collections.defaultdict` o `collections.OrderedDict` (antes de Python 3.7) podrían ser preferibles. `defaultdict` es particularmente útil cuando necesitas que un diccionario devuelva un valor predeterminado para claves no existentes, simplificando ciertos tipos de lógica condicional. Sin embargo, con la mejora y evolución continua de Python, la clase de diccionario incorporada a menudo sigue siendo la opción predilecta para los arreglos asociativos debido a su solidez y la comodidad que ofrece de manera inmediata.
