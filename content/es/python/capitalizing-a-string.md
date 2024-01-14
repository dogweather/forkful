---
title:    "Python: Capitalizando una cadena"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¡Por qué!

Capitalizar una cadena de texto puede ser útil en diversas situaciones, como por ejemplo al manipular nombres de usuarios o nombres propios en un programa. También puede ser útil para mejorar la legibilidad de un texto en mayúsculas y minúsculas.

## ¡Cómo hacerlo!

Para capitalizar una cadena de texto, podemos utilizar el método `capitalize()` en Python. Este método convertirá la primera letra de la cadena a mayúscula, dejando el resto de la cadena en minúsculas. Veamos un ejemplo en código:

```Python
cadena = "hola, soy python"
cadena = cadena.capitalize()
print(cadena)

# Output:
# Hola, soy python
```

También podemos utilizar el método `title()` para capitalizar cada palabra en una cadena de texto. Esto es útil cuando tenemos nombres compuestos o frases. Veamos un ejemplo:

```Python
cadena = "juan carlos"
cadena = cadena.title()
print(cadena)

# Output:
# Juan Carlos
```

## ¡Profundizando!

Además de los métodos mencionados anteriormente, Python también cuenta con el método `upper()` para convertir toda la cadena a mayúsculas y el método `lower()` para convertirla a minúsculas. Es importante tener en cuenta que estos métodos no solo afectan a las letras del alfabeto, sino que también convierten los caracteres como acentos y diéresis.

Otra cosa a tener en cuenta es que estos métodos no cambian la cadena original, sino que devuelven una nueva cadena con la modificación realizada. Por lo tanto, siempre debemos asignar el resultado a una variable o imprimirlo directamente.

## ¡Vea También!

- [Documentación oficial de Python sobre métodos de cadena](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Ejemplos de capitalización de cadenas en Python](https://www.w3schools.com/python/ref_string_capitalize.asp)
- [Tutorial de Python en español](https://www.programiz.com/python-programming)