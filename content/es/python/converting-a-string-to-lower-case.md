---
title:    "Python: Convirtiendo una cadena a minúsculas"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Por qué

Python es un lenguaje de programación versátil y potente, y una de sus características más útiles es su capacidad para trabajar con cadenas de texto. Una tarea común en el procesamiento de texto es convertir una cadena a minúsculas, lo que puede ser útil para comparar o limpiar datos. En este artículo, te enseñaré cómo convertir una cadena a minúsculas en Python.

# Cómo hacerlo

La forma más sencilla de convertir una cadena a minúsculas es utilizando el método `lower()` en la cadena. Este método devuelve una copia de la cadena en minúsculas.

```
Python
cadena = "HOLA MUNDO"
print(cadena.lower())

# Output: hola mundo
```

También puedes utilizar la función `casefold()`, que es similar al método `lower()` pero tiene en cuenta las diferencias regionales de los idiomas.

```
Python
cadena = "hola MUNDO"
print(cadena.casefold())

# Output: hola mundo
```

Otra forma de convertir una cadena a minúsculas es utilizando la función `str.lower()` si deseas trabajar con una variable que no es una cadena.

```
Python
variable = 123
print(str.lower(variable))

# Output: 123
```

# Profundizando más

Es importante tener en cuenta que los métodos `lower()` y `casefold()` no modifican la cadena original, sino que devuelven una nueva cadena en minúsculas. Esto significa que si deseas utilizar la cadena en minúsculas más adelante, debes asignarla a una nueva variable o sobrescribir la original.

Además, estos métodos solo funcionan con cadenas de caracteres ASCII. Si trabajas con caracteres no ASCII, como letras acentuadas o símbolos, es posible que necesites utilizar otras técnicas más avanzadas para convertirlos a minúsculas correctamente.

# Ver también

- [Documentación oficial sobre el método `lower()`] (https://docs.python.org/es/3/library/stdtypes.html#str.lower)
- [Cómo trabajar con cadenas en Python] (https://www.geeksforgeeks.org/python-strings/)
- [Ejemplos prácticos de procesamiento de texto en Python] (https://www.datacamp.com/community/tutorials/text-analytics-beginners-nltk)

¡Ahora ya sabes cómo convertir una cadena a minúsculas en Python! ¡Explora y diviértete trabajando con cadenas de texto en tus próximos proyectos!

__________________________________________

# Ver también

- [Documentación oficial sobre el método `lower()`] (https://docs.python.org/es/3/library/stdtypes.html#str.lower)
- [Cómo trabajar con cadenas en Python] (https://www.geeksforgeeks.org/python-strings/)
- [Ejemplos prácticos de procesamiento de texto en Python] (https://www.datacamp.com/community/tutorials/text-analytics-beginners-nltk)