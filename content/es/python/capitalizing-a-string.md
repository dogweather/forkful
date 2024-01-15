---
title:                "Majusculeando una cadena"
html_title:           "Python: Majusculeando una cadena"
simple_title:         "Majusculeando una cadena"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# ¿Por qué deberías capitalizar una cadena en Python?

Capitalizar una cadena en Python puede ser útil para mejorar la presentación y legibilidad de tus datos. Además, es una habilidad básica que te permitirá manipular y transformar eficientemente cadenas de texto en tus proyectos.

## Cómo hacerlo

```Python
# Guardamos una cadena en una variable
cadena = "hola mundo"

# Usando el método capitalize()
cadena_capitalizada = cadena.capitalize()

# Imprimimos el resultado
print(cadena_capitalizada)
```

Output:
```
Hola mundo
```

Como puedes ver en el ejemplo, simplemente debemos utilizar el método `capitalize()` en la cadena que queremos capitalizar y guardar el resultado en una nueva variable. Este método convierte la primera letra de la cadena en mayúscula y deja el resto en minúscula.

Otra forma de capitalizar una cadena en Python es utilizando el método `title()`, el cual convierte la primera letra de cada palabra en mayúscula. A continuación, un ejemplo de cómo utilizarlo:

```Python
cadena = "bienvenido a mi mundo"
cadena_capitalizada = cadena.title()
print(cadena_capitalizada)
```

Output:
```
Bienvenido A Mi Mundo
```

## Profundizando

Además de los métodos `capitalize()` y `title()`, también existen otras formas de capitalizar una cadena en Python según tus necesidades. Por ejemplo, si solo quieres capitalizar la primera letra de cada oración, puedes utilizar el módulo `string` y la función `capwords()` de la siguiente manera:

```Python
#Importamos el módulo string
import string

# Guardamos una cadena en una variable
cadena = "bienvenido a mi mundo"

# Usando la función capwords()
cadena_capitalizada = string.capwords(cadena)

# Imprimimos el resultado
print(cadena_capitalizada)
```

Output:
```
Bienvenido A Mi Mundo
```

Como puedes ver, el resultado es el mismo que al utilizar el método `title()`. Sin embargo, la ventaja de utilizar el módulo `string` y su función `capwords()` es que puedes especificar qué caracteres quieres considerar como separadores de palabras.

Por ejemplo, si en lugar de capitalizar la primera letra de cada palabra, solo quieres capitalizar la primera letra después de un guión, puedes hacer lo siguiente:

```Python
cadena = "bienvenido-a-mi-mundo"
cadena_capitalizada = string.capwords(cadena,"-")
print(cadena_capitalizada)
```

Output:
```
Bienvenido-A-Mi-Mundo
```

## Ver también

Si quieres seguir aprendiendo sobre el manejo de cadenas en Python, te recomiendo revisar los siguientes recursos:

- [Documentación oficial de Python sobre cadenas](https://docs.python.org/es/3.9/library/stdtypes.html#string-methods)
- [Python String Tutorial: Learn Strings in Python](https://www.datacamp.com/community/tutorials/python-string-tutorial)
- [Real Python: Python Strings and String Formatting Best Practices](https://realpython.com/python-strings/)