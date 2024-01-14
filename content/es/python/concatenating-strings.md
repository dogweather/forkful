---
title:                "Python: Uniendo cadenas de texto"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

Concatenar strings es una herramienta esencial en la programación de Python ya que nos permite combinar varias cadenas de caracteres en una sola. Esto puede ser útil en muchas situaciones, como la creación de mensajes personalizados, la manipulación de datos y la generación de reportes.

## Cómo hacerlo

El proceso de concatenación de strings en Python es bastante sencillo. Primero, debemos tener dos o más strings que queremos unir. Luego, utilizamos el operador `+` para concatenarlas juntas. Por ejemplo:

```Python
string1 = "Hola"
string2 = "mundo"
string3 = string1 + " " + string2
print(string3)
```

Este código producirá la salida "Hola mundo" al imprimir el valor de `string3`. Si queremos agregar más strings, simplemente seguimos utilizando el operador `+`. Por ejemplo:

```Python
string1 = "¡Hola!"
string2 = "¿Cómo estás?"
string3 = string1 + " " + string2 + " Yo estoy bien."
print(string3)
```

La salida de este código será "¡Hola! ¿Cómo estás? Yo estoy bien." También podemos utilizar la función `format()` para concatenar strings y variables juntos. Por ejemplo:

```Python
nombre = "María"
edad = 25
mensaje = "Hola, mi nombre es {} y tengo {} años.".format(nombre, edad)
print(mensaje)
```

La salida de este código será "Hola, mi nombre es María y tengo 25 años."

También podemos utilizar el método `join()` para concatenar listas de strings. Por ejemplo:

```Python
lista_nombres = ["Juan", "María", "Pedro"]
nombres_completos = ", ".join(lista_nombres)
print(nombres_completos)
```
La salida de este código será "Juan, María, Pedro". Como puedes ver, el método `join()` nos permite especificar el separador que deseamos utilizar entre los strings, en este caso una coma y un espacio.

## Profundización

Ahora que ya sabemos cómo concatenar strings en Python, es importante entender que este proceso puede ser más complejo en ciertas situaciones. Por ejemplo, si queremos concatenar una gran cantidad de strings, puede ser más eficiente utilizar la clase `StringIO`, que evita la creación de strings intermedios en memoria.

También es importante tener en cuenta que los strings son inmutables en Python, lo que significa que no podemos modificarlos una vez que han sido creados. Por esta razón, cuando concatenamos una gran cantidad de strings, se está creando una nueva cadena en memoria cada vez que utilizamos el operador `+`. En estos casos, es mejor utilizar la función `join()` o la clase `StringIO` para mejorar el rendimiento de nuestro código.

## Vea también

- [Documentación oficial de Python sobre strings](https://docs.python.org/es/3/tutorial/introduction.html#strings)
- [Explicación más detallada sobre la concatenación de strings en Python](https://www.geeksforgeeks.org/python-string-concatenation/)
- [Más ejemplos de uso de la función `join()`](https://www.w3schools.com/python/ref_string_join.asp)