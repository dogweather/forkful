---
title:                "Python: Buscando y reemplazando texto"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, existen muchas ocasiones en las que se necesita realizar cambios en grandes bloques de texto. Para hacerlo de manera rápida y eficiente, se utiliza la técnica de búsqueda y reemplazo de texto. En este artículo, aprenderemos cómo implementar esta técnica en Python y profundizaremos en su funcionamiento.

## Cómo

Para realizar una búsqueda y reemplazo de texto en Python, utilizaremos la función `replace()`. Esta función toma dos parámetros: la cadena de texto a ser buscada y la cadena de texto que se usará como reemplazo. Veamos un ejemplo:

```python
texto = "Hola, mi nombre es Juan."
nuevo_texto = texto.replace("Juan", "Pedro")
print(nuevo_texto)
```

En este caso, el resultado sería `Hola, mi nombre es Pedro.` Como puedes ver, hemos reemplazado la palabra "Juan" por "Pedro" en la cadena de texto original.

Si queremos realizar un reemplazo en todo el texto, podemos utilizar la función `replace()` en conjunto con la función `split()` para dividir la cadena de texto en diferentes palabras y luego unirlo nuevamente con la función `join()`. Veamos un ejemplo:

```python
texto = "Este es un ejemplo de cómo realizar un reemplazo de texto."
palabras = texto.split(" ")
nuevo_texto = "_".join(palabras).replace("ejemplo", "caso de uso")
print(nuevo_texto)
```

El resultado sería `Este_es_un_caso_de_uso_de_cómo_realizar_un_reemplazo_de_texto.` Como puedes ver, hemos reemplazado la palabra "ejemplo" por "caso de uso" y también hemos cambiado los espacios por guiones bajos para que el texto se vea más limpio.

## Deep Dive

Ahora que ya conocemos cómo realizar un reemplazo de texto en Python, es importante entender cómo funciona esta técnica en profundidad. La función `replace()` toma en cuenta mayúsculas y minúsculas, por lo que si queremos que la búsqueda sea insensible a las mayúsculas, podemos utilizar la función `casefold()`. Además, también podemos especificar el número máximo de reemplazos que se pueden realizar utilizando el parámetro `count`.

También es importante mencionar que la función `replace()` solo reemplaza la primera coincidencia que encuentre. Si queremos reemplazar todas las coincidencias en una cadena de texto, podemos utilizar la función `replace()` en conjunto con una expresión regular.

## Ver también

- Tutorial de Python sobre expresiones regulares: https://www.pythonforbeginners.com/regex/regular-expressions-in-python
- Documentación oficial de Python sobre la función `replace()`: https://docs.python.org/es/3/library/stdtypes.html#str.replace