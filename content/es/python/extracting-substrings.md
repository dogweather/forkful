---
title:                "Extrayendo subcadenas"
html_title:           "Python: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qué & Por qué?
La extracción de subcadenas se trata de obtener una parte específica de una cadena de texto más larga. Los programadores a menudo lo hacen para manipular y analizar cadenas de texto de manera más eficiente.

## Cómo:
```Python
# Creamos una cadena de texto
cadena = "Hola mundo!"

# Extraemos los primeros 4 caracteres
subcadena = cadena[0:4]

# Imprimimos la subcadena
print(subcadena)

# Salida: Hola
```

## Deep Dive:
La extracción de subcadenas ha existido desde los primeros días de la programación, ya que es una tarea muy común y útil. Además de usar la notación de corchetes en Python, también se pueden utilizar métodos como `split()` y `substring()` para lograr el mismo resultado. Sin embargo, estos métodos pueden ser más complejos y menos eficientes en ciertos casos.

## See Also:
- [Tutorial de extracción de subcadenas en Python](https://www.programiz.com/python-programming/methods/string/substring)
- [Documentación oficial de Python sobre manipulación de cadenas de texto](https://docs.python.org/es/3.8/library/stdtypes.html#text-sequence-type-str)