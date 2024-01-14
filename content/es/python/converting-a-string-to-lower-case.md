---
title:                "Python: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas?

Hay muchas situaciones en las que necesitamos trabajar con cadenas de texto en un programa, y a menudo es útil tenerlas en minúsculas. Esto puede ser para realizar búsquedas de manera más eficiente, comparar cadenas de manera más precisa o simplemente por preferencia estética. En este artículo, aprenderemos cómo convertir una cadena a minúsculas en Python.

## Cómo hacerlo

En Python, podemos convertir una cadena a minúsculas utilizando el método `lower()`. Veamos un ejemplo:

```Python
texto = "¡HOLA! ¿CÓMO ESTÁS?"

print(texto.lower())
```

Esto producirá una salida de `¡hola! ¿cómo estás?`, ya que todas las letras se convertirán a minúsculas. También podemos aplicar este método directamente sobre una cadena ingresada por el usuario:

```Python
texto = input("Ingresa una cadena: ")

print(texto.lower())
```

Si el usuario ingresa `HOLA MUNDO`, la salida será `hola mundo`.

## Profundizando

Aunque el método `lower()` es el más común y sencillo para convertir una cadena a minúsculas en Python, también podemos utilizar la función `capitalize()` para capitalizar la primera letra de la cadena y convertir el resto en minúsculas.

Otra técnica es utilizar la librería `string` y su función `maketrans`, que permite crear una tabla de conversión para transformar una cadena a minúsculas. Esto puede ser útil si necesitamos manipular caracteres específicos.

Es importante tener en cuenta que al convertir una cadena a minúsculas, se afectan todos los caracteres que corresponden a letras en el alfabeto inglés. Los símbolos y caracteres especiales no se ven afectados por esta conversión.

## Ver también

- [Documentación de Python sobre el método lower()](https://docs.python.org/es/3/library/stdtypes.html#str.lower)
- [Tutorial de Real Python sobre métodos de cadenas en Python](https://realpython.com/python-strings/)
- [Documentación de Python sobre la función capitalize()](https://docs.python.org/es/3/library/stdtypes.html#str.capitalize)