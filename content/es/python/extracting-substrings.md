---
title:                "Python: Extrayendo subcadenas"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/extracting-substrings.md"
---

{{< edit_this_page >}}

# Por qué extraer subcadenas en Python

Extraer subcadenas, también conocido como subconjuntos de una cadena más grande, es una habilidad esencial en la programación de Python. Esto permite a los usuarios manipular y manejar texto de manera más eficiente, lo que puede ser especialmente útil en tareas como el procesamiento de texto o la limpieza de datos.

## Cómo hacerlo
Usando la función de corte de Python `string[start:end]`, podemos especificar qué subcadena queremos extraer de una cadena más grande. Por ejemplo, si tenemos la cadena "Hola Mundo", podemos extraer la subcadena "Mundo" usando `string[5:]`.

```Python
cadena = "Hola Mundo"
subcadena = cadena[5:]
print(subcadena) # Output: Mundo
```

También podemos especificar el inicio y el fin de la subcadena utilizando índices. Por ejemplo, si queremos extraer solo la letra "a" de la palabra "Python", podemos hacerlo así:`string[4]`.

```Python
cadena = "Python"
subcadena = cadena[4]
print(subcadena) # Output: a
```

## Profundizando
La función de corte de Python también nos permite especificar un tercer parámetro: el salto. Esto nos permite extraer subcadenas con un patrón específico. Por ejemplo, si queremos extraer solo las vocales de la palabra "Extraer", podemos hacerlo así: `string[::2]`.

```Python
cadena = "Extraer"
subcadena = cadena[::2]
print(subcadena) # Output: Erar
```

Además, también podemos usar números negativos en los parámetros de inicio y fin para especificar la subcadena desde el final de la cadena. Por ejemplo, si queremos extraer los últimos tres caracteres de la palabra "Python", podemos hacerlo así: `string[-3:]`.

```Python
cadena = "Python"
subcadena = cadena[-3:]
print(subcadena) # Output: hon
```

## Ver también
Si quieres seguir aprendiendo sobre la manipulación de cadenas en Python, estos enlaces pueden ser útiles:

- [Tutorial de manipulación de cadenas de programiz](https://www.programiz.com/python-programming/string)
- [Documentación oficial de Python sobre cadenas](https://docs.python.org/es/3/library/string.html)

¡Sigue practicando y pronto te convertirás en un experto en la extracción de subcadenas en Python!