---
title:                "Python: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

#¿Por qué capitalizar una cadena en Python?

Capitalizar una cadena en Python es una tarea común en la programación, especialmente en el procesamiento de texto. Puede ser útil cuando se desea estandarizar la capitalización de una cadena para facilitar la comparación o la presentación de datos.

## Cómo hacerlo

En Python, hay dos formas de capitalizar una cadena: utilizando el método `capitalize()` y el método `title()`. Ambos métodos devuelven una nueva cadena en mayúscula, pero tienen diferencias en la forma en que capitalizan la cadena original.

Usando el método `capitalize()`, solo se capitaliza el primer carácter de la cadena. Por ejemplo:

```Python
cadena = "hola mundo"
capitalizada = cadena.capitalize()
print(capitalizada) # "Hola mundo"
```

Mientras que, con el método `title()`, se capitaliza la primera letra de cada palabra en la cadena. Por ejemplo:

```Python
cadena = "hola mundo"
capitalizada = cadena.title()
print(capitalizada) # "Hola Mundo"
```

Ambos métodos son útiles en diferentes situaciones, por lo que es importante conocerlos y elegir el adecuado según sea necesario.

## Profundizando

Si deseas comprender mejor cómo funcionan estos métodos, es importante tener en cuenta que en Python, las cadenas son inmutables, lo que significa que no pueden ser modificadas directamente. Por lo tanto, cuando utilizas los métodos `capitalize()` o `title()`, en realidad se crea una nueva cadena con la capitalización deseada, mientras que la cadena original permanece intacta.

También es importante mencionar que estos métodos solo capitalizan letras, por lo que si la cadena contiene caracteres especiales, números o espacios, no se verán afectados. Además, si la cadena ya está completamente en mayúscula, no habrá cambios.

En resumen, capitalizar una cadena en Python es útil en muchas situaciones y existen diferentes métodos para lograrlo según tus necesidades.

## Ver También

- Documentación oficial de Python sobre el método `capitalize()`: https://docs.python.org/es/3/library/stdtypes.html#str.capitalize
- Documentación oficial de Python sobre el método `title()`: https://docs.python.org/es/3/library/stdtypes.html#str.title