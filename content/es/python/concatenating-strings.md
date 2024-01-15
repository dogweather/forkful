---
title:                "Concatenando cadenas"
html_title:           "Python: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué concatenar cadenas con Python?

Concatenar cadenas es una técnica útil en programación para combinar múltiples cadenas de texto en una sola. Esto puede ser útil en diferentes situaciones, como crear mensajes personalizados, generar nombres de archivos dinámicamente o construir consultas de bases de datos.

## Cómo hacerlo

La forma más sencilla de concatenar cadenas en Python es utilizando el operador de suma (+). Veamos un ejemplo:

```Python
name = "Sofía"
greeting = "Hola " + name + "!"
print(greeting)
```

Este código imprimirá "Hola Sofía!" en la consola. También podemos utilizar el método "format()" para concatenar cadenas. Veamos otro ejemplo:

```Python
fruit = "manzana"
color = "roja"
sentence = "Mi fruta favorita es la {} {}.".format(color, fruit)
print(sentence)
```

En este caso, el método "format()" nos permite insertar las variables "color" y "fruta" en la cadena, creando así la oración "Mi fruta favorita es la manzana roja."

## Profundizando en la concatenación de cadenas

Además de utilizar el operador de suma y el método "format()", Python ofrece otras funciones y métodos para concatenar cadenas. Algunos de ellos son "join()", "split()", "replace()" y "format_map()". Cada uno de estos tiene su propia funcionalidad y puede ser útil en diferentes situaciones.

También es importante mencionar que, en Python, las cadenas son inmutables, lo que significa que no se pueden modificar una vez creadas. Esto significa que cada vez que concatenamos cadenas, se crea una nueva cadena en lugar de modificar la existente.

## Ver también

- [Documentación oficial de Python sobre cadenas](https://docs.python.org/es/3/library/string.html)
- [Ejemplos de concatenación de cadenas en Python](https://realpython.com/python-string-concatenation/)
- [Tutorial de concatenación de cadenas de Programiz](https://www.programiz.com/python-programming/methods/string/join)