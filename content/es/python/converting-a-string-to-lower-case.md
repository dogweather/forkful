---
title:                "Python: Convirtiendo una cadena a minúsculas"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena de texto a minúsculas puede ser útil en muchas situaciones de programación, como comparar cadenas de texto sin tener en cuenta las mayúsculas y minúsculas, normalizar datos o simplemente por preferencia personal en la presentación del texto.

## Cómo hacerlo

Para convertir una cadena de texto a minúsculas en Python, podemos utilizar el método `lower()` que se encuentra disponible en los objetos de tipo `str`. Veamos un ejemplo:

```Python
palabra = "HOLA MUNDO"
print(palabra.lower())
```

**Output:** hola mundo

Como podemos ver, el método `lower()` convierte toda la cadena de texto a minúsculas, permitiendo así compararla con otras cadenas de texto sin importar si están escritas en mayúsculas o minúsculas.

También podemos utilizar el operador de asignación `+=` junto con el método `lower()` para modificar una cadena de texto existente en lugar de crear una nueva:

```Python
nombre = "Juan"
nombre += " "
nombre += "Pérez"
print(nombre.lower())
```

**Output:** juan pérez

## Profundizando

Es importante tener en cuenta que el método `lower()` solo funciona correctamente con caracteres ASCII. Si nuestra cadena de texto contiene caracteres especiales de otros idiomas, podemos usar la función `casefold()` en su lugar.

Además, debemos tener en cuenta que el método `lower()` no afecta a los caracteres de puntuación ni a los espacios en blanco, solo convierte las letras a minúsculas.

Si queremos convertir una cadena de texto a mayúsculas en lugar de minúsculas, podemos utilizar el método `upper()`, que funciona de manera similar al `lower()` pero convierte todas las letras a mayúsculas.

## Ver también

- [Método lower() en la documentación oficial de Python](https://docs.python.org/es/3/library/stdtypes.html#str.lower)
- [Función casefold() en la documentación oficial de Python](https://docs.python.org/es/3/library/stdtypes.html#str.casefold)
- [Operador de asignación += en la documentación oficial de Python](https://docs.python.org/es/3/library/stdtypes.html#last-example)