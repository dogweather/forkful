---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "C\xF3mo hacerlo: ."
lastmod: '2024-04-04T01:27:54.244490-06:00'
model: gpt-4-0125-preview
summary: .
title: "Eliminando caracteres que coinciden con un patr\xF3n"
weight: 5
---

## Cómo hacerlo:
```Python
import re

# Texto de ejemplo
text = "Hello, World! 1234"

# Eliminar todos los dígitos
sin_digitos = re.sub(r'\d', '', text)
print(sin_digitos)  # Salida: "Hello, World! "

# Eliminar puntuación
sin_puntuacion = re.sub(r'[^\w\s]', '', text)
print(sin_puntuacion)  # Salida: "Hello World 1234"

# Eliminar vocales
sin_vocales = re.sub(r'[aeiouAEIOU]', '', text)
print(sin_vocales)  # Salida: "Hll, Wrld! 1234"
```

### Una función personalizada que escribí

Lo hago con suficiente frecuencia como para haber refactorizado esto en la función `eliminar()`. También es una buena demostración de [doctests](https://docs.python.org/3/library/doctest.html):

```python
def eliminar(cadena: str, regex: str) -> str:
    """
    >>> eliminar("Hello, world!", "l")
    'Heo, word!'

    >>> eliminar("Hello, world!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", cadena)
```



## Análisis profundo
La práctica de eliminar caracteres que coinciden con un patrón en el texto tiene profundas raíces en la ciencia de la computación, remontándose a herramientas Unix tempranas como `sed` y `grep`. En Python, el módulo `re` proporciona esta capacidad, aprovechando las expresiones regulares—una herramienta poderosa y versátil para el procesamiento de texto.

Alternativas al módulo `re` incluyen:
- Métodos string como `replace()` para casos simples.
- Librerías de terceros como `regex` para patrones más complejos y mejor soporte de Unicode.

Entre bastidores, cuando usas `re.sub()`, el intérprete de Python compila el patrón en una serie de códigos de bytes, procesados por una máquina de estado que realiza el emparejamiento de patrones directamente en el texto de entrada. Esta operación puede ser intensiva en recursos para cadenas grandes o patrones complejos, por lo tanto, las consideraciones de rendimiento son cruciales para el procesamiento de grandes volúmenes de datos.

## Ver también
- [Documentación del módulo `re` de Python](https://docs.python.org/3/library/re.html): Documentos oficiales para expresiones regulares en Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): Una guía completa para expresiones regulares.
- [Tutorial de Real Python sobre regex](https://realpython.com/regex-python/): Aplicaciones del mundo real de expresiones regulares en Python.
