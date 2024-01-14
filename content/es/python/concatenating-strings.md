---
title:                "Python: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

El concatenar cadenas de texto es una habilidad clave en la programación de Python, ya que te permite combinar varias cadenas en una sola, creando mensajes personalizados y estructuras de código más dinámicas y flexibles.

## Cómo

Para concatenar cadenas en Python, utilizamos el operador `+`. Veamos un ejemplo sencillo:

```Python
nombre = "Juan"
apellido = "Pérez"
nombre_completo = nombre + " " + apellido

print(nombre_completo)
```

El código anterior imprimirá "Juan Pérez" en la consola. Como se puede observar, utilizamos el operador `+` para combinar las diferentes cadenas en una sola.

Pero, ¿qué pasa si queremos agregar un número al final de la cadena? En ese caso, tendríamos que convertir el número a una cadena utilizando la función `str()`:

```Python
mensaje = "El número de teléfono de " + nombre + " es " + str(numero)

print(mensaje)
```

La salida sería "El número de teléfono de Juan es 123456789". Como se puede ver, utilizamos la función `str()` para convertir el número a una cadena antes de concatenarlo con las otras cadenas.

También podemos utilizar la función `format()` para realizar concatenaciones en una forma más legible y estructurada. Veamos un ejemplo:

```Python
edad = 25
mensaje = "Mi nombre es {} y tengo {} años".format(nombre, edad)

print(mensaje)
```

En este caso, utilizamos llaves `{}` como espacios reservados para las variables, y luego utilizamos la función `format()` para reemplazar esas llaves con los valores de las variables.

## Deep Dive

La función `format()` también nos permite especificar el formato en el que queremos mostrar las variables. Por ejemplo, podemos especificar cuántos decimales queremos mostrar para un número:

```Python
precio = 50.99
mensaje = "El precio es de {:.2f} dólares".format(precio)

print(mensaje)
```

La salida sería "El precio es de 50.99 dólares", ya que utilizamos `:.2f` para indicar que queremos mostrar solo dos decimales del precio.

Otra forma de concatenar cadenas es utilizando el operador `%`, que funciona de manera similar a la función `format()`. Veamos un ejemplo:

```Python
peso = 75.6
mensaje = "Mi nombre es %s y peso %.1f kilogramos" % (nombre, peso)

print(mensaje)
```

En este caso, utilizamos `%s` para indicar que queremos insertar una cadena y `%f` para indicar un número con decimales. Luego, especificamos los valores correspondientes entre paréntesis.

## Ver también

- [Documentación oficial de Python sobre concatenación de cadenas](https://docs.python.org/es/3/tutorial/introduction.html#concatenation)
- [Tutorial de W3Schools sobre concatenación de cadenas en Python](https://www.w3schools.com/python/python_strings_concatenate.asp)
- [Artículo de Programación Python sobre formateo de cadenas](https://www.programacionpython.com/formatear-cadenas-en-python/)