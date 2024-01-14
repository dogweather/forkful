---
title:                "Python: Cambiando a mayúsculas una cadena"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Por qué capitalizar una cadena en Python

Muchas veces, cuando trabajamos con cadenas de texto en Python, nos encontramos con la necesidad de capitalizarlas, es decir, convertir la primera letra de cada palabra en mayúscula. Esto puede ser útil para darle un formato adecuado a nuestros datos o para mejorar la apariencia de nuestro código. En este artículo, aprenderemos por qué y cómo capitalizar una cadena en Python.

## Cómo hacerlo

Para capitalizar una cadena en Python, podemos utilizar el método `capitalize()` de la clase `str`. Este método toma la cadena original y devuelve una nueva cadena con la primera letra en mayúscula y el resto en minúscula. Veamos un ejemplo:

```Python
cadena = "python es un lenguaje de programación"
capitalizada = cadena.capitalize()
print(capitalizada)
```

El resultado de este código será:

```
Python es un lenguaje de programación
```

Podemos utilizar este método también en cadenas que contengan palabras con mayúsculas, e incluso en cadenas que contengan caracteres especiales o números.

```Python
cadena = "8 pythonistas en una sala de 80"
capitalizada = cadena.capitalize()
print(capitalizada)
```

El resultado de este código será:

```
8 pythonistas en una sala de 80
```

Como puedes ver, el método `capitalize()` solo convierte la primera letra de la cadena a mayúscula y deja las demás letras como estaban.

## Un poco más profundo

El método `capitalize()` puede ser útil para capitalizar una cadena en casos simples, pero si queremos capitalizar todas las palabras de una cadena, incluyendo aquellas que ya tienen mayúsculas, es posible que necesitemos una solución más avanzada.

Una forma de hacerlo sería utilizando el método `title()`, que capitaliza cada palabra de una cadena. Sin embargo, este método también convierte todas las letras a minúsculas, por lo que tendríamos que convertir nuevamente la primera letra de cada palabra a mayúscula. Para evitar esto, podemos utilizar la función `capitalize()` junto con la función `upper()` para convertir la primera letra de cada palabra a mayúscula, manteniendo las demás letras intactas:

```Python
cadena = "python es un lenguaje de programación"
capitalizada = cadena.title().upper()
print(capitalizada)
```

El resultado de este código será:

```
PYTHON ES UN LENGUAJE DE PROGRAMACIÓN
```

Otra opción es utilizar expresiones regulares para identificar cada palabra de la cadena y convertir su primera letra a mayúscula. Sin embargo, esto puede ser un poco más complejo y está fuera del alcance de este artículo.

# Ver también

- Documentación oficial de Python sobre el método `capitalize()`: https://docs.python.org/es/3/library/stdtypes.html#str.capitalize
- Ejemplos de uso del método `capitalize()`: https://www.programiz.com/python-programming/methods/string/capitalize