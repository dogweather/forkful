---
title:    "Python: Encontrar la longitud de una cadena"
keywords: ["Python"]
---

{{< edit_this_page >}}

## ¿Por qué deberías aprender a encontrar la longitud de una cadena en Python?

En la programación, a menudo es necesario trabajar con cadenas de texto. Una de las tareas más comunes es encontrar la longitud de una cadena, es decir, el número de caracteres en una cadena. Aprender a hacer esto en Python puede ahorrarte mucho tiempo y es una habilidad esencial para cualquier programador.

## Cómo encontrar la longitud de una cadena en Python

Para encontrar la longitud de una cadena en Python, podemos utilizar la función `len()`. Por ejemplo, si tenemos la siguiente cadena:

```Python
frase = "Hola, ¿cómo estás?"
```

Podemos encontrar su longitud usando `len()` de la siguiente manera:

```Python
print(len(frase))
```

Esto nos dará un resultado de 18, ya que hay 18 caracteres en la cadena. También podemos usar `len()` en una cadena vacía, lo que nos dará un resultado de 0.

## Profundizando en la longitud de una cadena

Ahora que sabemos cómo usar `len()` para encontrar la longitud de una cadena, veamos algunos ejemplos más complejos:

- Podemos utilizar `len()` en cadenas que contengan caracteres especiales o emojis.
- Si la cadena contiene espacios, también se contarán como caracteres.
- Incluso podemos encontrar la longitud de una cadena que contiene números.

También es importante tener en cuenta que `len()` no solo funciona en cadenas, también podemos usarlo en listas, tuplas, diccionarios y otros tipos de datos.

## Ver También

- [La documentación oficial de Python sobre `len()` en español](https://docs.python.org/es/3/library/functions.html#len)
- [Ejemplos de `len()` en la vida real](https://realpython.com/python-len/#using-len-on-different-types)