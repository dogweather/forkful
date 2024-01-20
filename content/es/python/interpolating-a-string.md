---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolación de Cadenas en Python

## ¿Qué & Por qué?

La interpolación de cadenas es un método para insertar (o sustituir) variables en un string. Los programadores lo hacen para construir dinámicamente strings de una forma legible y eficiente.

## ¿Cómo hacerlo?

Python ofrece amplias formas de realiza la interpolación de cadenas, vamos a ver tres de esas formas aquí.

### Usando el método format()

```python
nombre = "Ana"
print("Hola, {}!".format(nombre))
```
OUTPUT:
```
Hola, Ana!
```
### Utilizando una f String

```python
nombre = "Ana"
print(f"Hola, {nombre}!")
```
OUTPUT:
```
Hola, Ana!
```
### Usando el operador %

```python
nombre = "Ana"
print("Hola, %s!" % nombre)
```
OUTPUT:
```
Hola, Ana!
```

## Profundización 

La interpolación de cadenas ha sido una característica de Python desde sus primeros días. El operador `%`, también conocido como "formato de estilo antiguo", es la primera forma de interpolación de cadenas en Python. Sin embargo, este enfoque se considera menos pythonico que los otros dos métodos mencionados.

A partir de Python 3.6, las f-strings se introdujeron como una forma más legible y eficiente de interpolación de cadenas. Son generalmente más rápidas y permiten una expresión en línea elegante.

El método `format()` es una alternativa útil de interpolación de cadenas, particularmente si estás trabajando con una versión de Python inferior a 3.6. Sin embargo, en comparación con las f-strings, no son tan rápidas ni tan legibles.

La elección entre estos métodos depende en gran medida de la versión de Python que estés utilizando, y de tus requisitos de legibilidad y rendimiento.

## Ver También 

Para obtener más información sobre la interpolación de cadenas en Python y las recomendaciones de uso, consulte los siguientes recursos:

- [Documentación oficial de Python: Interpolación de Cadenas](https://docs.python.org/3/tutorial/inputoutput.html)
- [PEP 3101- Nueva forma de formatos String en Python](https://peps.python.org/pep-3101/)
- [PEP 498 - Literales de cadena formateados(f-strings)](https://peps.python.org/pep-0498/)
- [Tutorial Interpolación de Python por Real Python](https://realpython.com/python-f-strings/)