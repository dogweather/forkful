---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:49.564032-07:00
description: "C\xF3mo hacerlo: Usar regex en Python implica el m\xF3dulo `re`, que\
  \ proporciona un conjunto de funciones para procesar texto utilizando expresiones\
  \ regulares."
lastmod: '2024-04-05T21:53:59.959727-06:00'
model: gpt-4-0125-preview
summary: "Usar regex en Python implica el m\xF3dulo `re`, que proporciona un conjunto\
  \ de funciones para procesar texto utilizando expresiones regulares."
title: Usando expresiones regulares
weight: 11
---

## Cómo hacerlo:
Usar regex en Python implica el módulo `re`, que proporciona un conjunto de funciones para procesar texto utilizando expresiones regulares.

### Coincidencia de Patrones Básica
Para buscar un patrón en una cadena, usa `re.search()`. Retorna un objeto de coincidencia cuando el patrón es encontrado, de lo contrario `None`.
```python
import re

texto = "Aprende programación con Python"
coincidencia = re.search("Python", texto)
if coincidencia:
    print("¡Patrón encontrado!")
else:
    print("Patrón no encontrado.")
```
Salida:
```
¡Patrón encontrado!
```

### Compilando Expresiones Regulares
Para el uso repetido del mismo patrón, compílalo primero con `re.compile()` para un mejor rendimiento.
```python
patron = re.compile("Python")
coincidencia = patron.search("Aprende programación con Python")
if coincidencia:
    print("¡Patrón compilado encontrado!")
```
Salida:
```
¡Patrón compilado encontrado!
```

### Dividiendo Cadenas
Para dividir una cadena en cada coincidencia de un patrón de regex, usa `re.split()`.
```python
resultado = re.split("\s", "Python es divertido")
print(resultado)
```
Salida:
```
['Python', 'es', 'divertido']
```

### Encontrando Todas las Coincidencias
Para encontrar todas las ocurrencias no superpuestas de un patrón, usa `re.findall()`.
```python
coincidencias = re.findall("n", "Programación con Python")
print(coincidencias)
```
Salida:
```
['n', 'n']
```

### Reemplazando Texto
Usa `re.sub()` para reemplazar ocurrencias de un patrón con una nueva cadena.
```python
texto_reemplazado = re.sub("divertido", "asombroso", "Python es divertido")
print(texto_reemplazado)
```
Salida:
```
Python es asombroso
```

### Bibliotecas de Terceros
Aunque el módulo `re` integrado en Python es poderoso, las bibliotecas de terceros como `regex` ofrecen más características y un rendimiento mejorado. Para usar `regex`, instálalo vía pip (`pip install regex`) e impórtalo en tu código.

```python
import regex

texto = "Aprendiendo Python 3.8"
coincidencia = regex.search(r"Python\s(\d+\.\d+)", texto)
if coincidencia:
    print(f"Versión encontrada: {coincidencia.group(1)}")
```
Salida:
```
Versión encontrada: 3.8
```
