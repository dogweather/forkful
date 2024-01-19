---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/extracting-substrings.md"
---

{{< edit_this_page >}}

# **Extraer subcadenas en Python: una guía rápida**

## **¿Qué y por qué?**
La extracción de subcadenas implica tomar una parte específica de una cadena de texto. Los programadores lo hacen regularmente para analizar datos, manipular texto y realizar tareas similares.

## **Cómo se hace:**

Extraer una subcadena en Python es simple. Utilizamos los índices para hacerlo. Aquí te deja un ejemplo,

```Python
frase = 'Hola mundo!'
primer_palabra = frase[0:4]  # Selecciona los caracteres del 0 al 3, índice 4 no inclusive
print(primer_palabra)
```

Salida:
```Python
'Hola'
```

Python también nos permite usar índices negativos para contar desde el final.

```Python
ultima_letra = frase[-1]  # Último carácter 
print(ultima_letra)
```
Salida:

```Python
'!'
```

## **Inmersión profunda:**

Históricamente, la extracción de subcadenas se originó con la necesidad de manejar cadenas de texto en lenguajes de programación primitivos. Con el tiempo, Python ha simplificado enormemente este proceso con su sistema de indexación.

Aunque hemos visto cómo hacerlo usando índices directamente, también existen otras maneras de extraer subcadenas, como el método `slice()` y el método `split()`. Ambos pueden ser más adecuados dependiendo de tus necesidades específicas.

En términos de implementación, Python utiliza una estructura de datos conocida como array de caracteres para almacenar cadenas. Cada carácter en la cadena tiene un índice asociado, que se puede utilizar para acceder y extraer ese carácter.

## **Ver también:**

Para más información, te recomiendo las siguientes fuentes:

- [W3Schools: Python Strings](https://www.w3schools.com/python/python_strings.asp)
- [Real Python: An Overview of Python’s Datatypes](https://realpython.com/python-data-types/)
- [Python Documentation: Built-in Types](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)