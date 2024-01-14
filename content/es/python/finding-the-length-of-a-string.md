---
title:    "Python: Encontrando la longitud de una cadena"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué encontrar la longitud de una cadena en Python?

En la programación de Python, es común encontrarse con cadenas de texto. Estas cadenas pueden variar en longitud y a veces es necesario determinar cuántos caracteres hay en una cadena. En este artículo, aprenderemos cómo encontrar la longitud de una cadena en Python y por qué es importante.

## Cómo encontrar la longitud de una cadena en Python

En Python, podemos usar la función `len()` para encontrar la longitud de una cadena. Por ejemplo:

```Python
cadena = "¡Hola Mundo!"
longitud = len(cadena)
print(longitud)
```

La salida de este código será `12`, ya que hay 12 caracteres en la cadena "¡Hola Mundo!".

También podemos usar la función `len()` en variables que almacenan cadenas ingresadas por el usuario:

```Python
nombre = input("Por favor ingrese su nombre: ")
longitud = len(nombre)
print("La longitud de su nombre es:", longitud)
```

La salida de este código será la longitud del nombre que el usuario ingresó.

## Profundizando en encontrar la longitud de una cadena

Es importante notar que la función `len()` solo cuenta el número de caracteres en una cadena, incluyendo espacios y signos de puntuación. También incluye caracteres especiales como letras acentuadas o símbolos.

Otra cosa a tener en cuenta es que la función `len()` también se puede utilizar en otras estructuras de datos, como listas, tuplas y diccionarios, para encontrar su longitud.

En resumen, determinar la longitud de una cadena en Python es útil cuando necesitamos manipular y analizar datos en forma de texto.

## Ver también

- [Tutorial de cadenas en Python](https://www.python.org/dev/peps/pep-0008/#strings)
- [Documentación oficial de Python sobre la función len()](https://docs.python.org/3/library/functions/html#len)