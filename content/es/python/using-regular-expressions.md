---
title:                "Uso de expresiones regulares"
date:                  2024-01-19
html_title:           "Arduino: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Las expresiones regulares (regex) son patrones usados para encontrar coincidencias y manipular cadenas de texto. Programadores las usan por su poder y flexibilidad para buscar y reemplazar texto de forma precisa y eficiente.

## Cómo hacerlo:

Aquí tienes ejemplos prácticos:

```Python
import re

# Encontrar todas las coincidencias de 'python'
texto = "Python 3.8 es impresionante. python hace la programación divertida."
coincidencias = re.findall('python', texto, re.IGNORECASE)
print(coincidencias)  # ['Python', 'python']

# Reemplazar 'python' por 'Java'
texto_reemplazado = re.sub('python', 'Java', texto, flags=re.IGNORECASE)
print(texto_reemplazado)  # Java 3.8 es impresionante. Java hace la programación divertida.

# Validar formato de correo electrónico
correo = "usuario@example.com"
es_valido = re.fullmatch(r"[^@]+@[^@]+\.[^@]+", correo)
print(es_valido)  # <re.Match object; span=(0, 19), match='usuario@example.com'>
```

## Profundización

Las expresiones regulares surgieron en los años 50 y tienen sus raíces en la teoría de autómatas y lenguajes formales. Alternativas a regex incluyen trabajar con métodos de cadenas en Python, como `.find()` y `.replace()`, o utilizar bibliotecas de parsing especializadas según el contexto, como `BeautifulSoup` para HTML. En cuanto a implementación, Python utiliza la librería `re`, que ofrece una interfaz para trabajar con expresiones regulares aprovechando el motor de regex de Perl.

## Ver También

Para profundizar en expresiones regulares y su uso en Python, estos enlaces son útiles:

- Documentación oficial: https://docs.python.org/3/library/re.html
- Tutorial de regex en Python: https://realpython.com/regex-python/
- Expresiones regulares para principiantes: https://www.regular-expressions.info/
