---
title:                "Python: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué
Extraer subcadenas es una habilidad útil en programación que te permite obtener una parte específica de un texto más grande. Puedes usar esta técnica para manipular y analizar datos, así como para mejorar la presentación de resultados en tus programas.

## Cómo hacerlo
Para extraer subcadenas en Python, simplemente usa el método `substring()` en una cadena de texto seguido de los índices de inicio y fin del texto que deseas obtener. Aquí hay un ejemplo de código:

```Python
texto = "¡Hola a todos!"
print(texto.substring(0,4))
```

Esto imprimirá "¡Hola", ya que los índices van desde 0 hasta 3, extrayendo la primera parte de la cadena original. También puedes usar índices negativos para contar desde el final de la cadena hacia atrás. Por ejemplo:

```Python
texto = "¡Hola a todos!"
print(texto.substring(-6,-1))
```

Esto imprimirá "todos", ya que los índices negativos empiezan desde -1 en lugar del típico índice 0. Puedes jugar con diferentes índices para obtener diferentes subcadenas según tus necesidades.

## Profundizando
El método `substring()` en Python funciona de manera similar al método `slice()` y también admite un tercer parámetro opcional para indicar el paso de la extracción. Pero a diferencia del método `slice()`, `substring()` no permite índices negativos para indicar el final de la extracción. También debes tener cuidado de no usar índices fuera del rango de la cadena original para evitar errores.

## Ver también
- [Documentación oficial de Python sobre extraer subcadenas](https://docs.python.org/es/3.9/library/stdtypes.html#string-methods)
- [Ejemplos de código para extraer subcadenas en Python](https://www.geeksforgeeks.org/python-ways-to-extract-partial-strings-from-string/)