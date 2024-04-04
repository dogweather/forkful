---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "C\xF3mo hacerlo: ."
lastmod: '2024-04-04T02:02:37.479626-06:00'
model: gpt-4-0125-preview
summary: .
title: "Eliminaci\xF3n de caracteres que coinciden con un patr\xF3n"
weight: 5
---

## Cómo hacerlo:
```Python
import re

# Cadena de ejemplo
text = "Hola, Mundo! 1234"

# Eliminar todos los dígitos
no_digitos = re.sub(r'\d', '', text)
print(no_digitos)  # Salida: "Hola, Mundo! "

# Eliminar puntuación
sin_puntuacion = re.sub(r'[^\w\s]', '', text)
print(sin_puntuacion)  # Salida: "Hola Mundo 1234"

# Eliminar vocales
sin_vocales = re.sub(r'[aeiouAEIOU]', '', text)
print(sin_vocales)  # Salida: "Hl, Mnd! 1234"
```

### Mi función personalizada

Lo hago con suficiente frecuencia como para refactorizarlo en esta simple función `delete()`. También es una buena demostración de [doctests](https://docs.python.org/3/library/doctest.html):

```python
def delete(cadena: str, regex: str) -> str:
    """
    >>> delete("Hola, mundo!", "l")
    'Hoa, mundo!'

    >>> delete("Hola, mundo!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", cadena)
```



## Profundizando
La práctica de eliminar caracteres que coincidan con un patrón en el texto tiene raíces profundas en la informática, remontándose a herramientas Unix tempranas como `sed` y `grep`. En Python, el módulo `re` proporciona esta capacidad, aprovechando las expresiones regulares—una herramienta poderosa y versátil para el procesamiento de texto.

Alternativas al módulo `re` incluyen:
- Métodos de cadena como `replace()` para casos simples.
- Bibliotecas de terceros como `regex` para patrones más complejos y mejor soporte Unicode.

Bajo el capó, cuando usas `re.sub()`, el intérprete de Python compila el patrón en una serie de códigos de operación, procesados por una máquina de estados que realiza el coincidencia de patrones directamente en el texto de entrada. Esta operación puede ser intensiva en recursos para cadenas grandes o patrones complejos, por lo que las consideraciones de rendimiento son cruciales para el procesamiento de grandes volúmenes de datos.

## Ver también
- [Documentación del módulo `re` de Python](https://docs.python.org/3/library/re.html): Documentación oficial para expresiones regulares en Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): Una guía completa sobre expresiones regulares.
- [Tutorial de Real Python sobre regex](https://realpython.com/regex-python/): Aplicaciones del mundo real de las expresiones regulares en Python.
