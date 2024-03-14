---
date: 2024-01-26 04:44:21.241110-07:00
description: "Los n\xFAmeros complejos son un conjunto de n\xFAmeros de la forma `a\
  \ + bi`, donde `a` y `b` son n\xFAmeros reales, e `i` es la unidad imaginaria (`i^2\
  \ = -1`). En\u2026"
lastmod: '2024-03-13T22:44:58.604571-06:00'
model: gpt-4-0125-preview
summary: "Los n\xFAmeros complejos son un conjunto de n\xFAmeros de la forma `a +\
  \ bi`, donde `a` y `b` son n\xFAmeros reales, e `i` es la unidad imaginaria (`i^2\
  \ = -1`). En\u2026"
title: "Trabajando con n\xFAmeros complejos"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Los números complejos son un conjunto de números de la forma `a + bi`, donde `a` y `b` son números reales, e `i` es la unidad imaginaria (`i^2 = -1`). En programación, los utilizamos para resolver problemas en varios dominios, como ingeniería eléctrica, procesamiento de señales y computación cuántica.

## Cómo hacerlo:
Python tiene soporte integrado para números complejos. Así es como puedes jugar con ellos:

```Python
# Creando números complejos
z = 4 + 5j
print(z)  # Salida: (4+5j)

# Accediendo a las partes real e imaginaria
print(z.real)  # Salida: 4.0
print(z.imag)  # Salida: 5.0

# Aritmética compleja
w = 1 - 2j
print(z + w)  # Salida: (5+3j)
print(z - w)  # Salida: (3+7j)
print(z * w)  # Salida: (14+2j)
print(z / w)  # Salida: (-3.6+1.2j)

# Módulo (valor absoluto)
print(abs(z))  # Salida: 6.4031242374328485

# Conjugado de un número complejo
print(z.conjugate())  # Salida: (4-5j)
```

## Estudio Profundo
Los números complejos fueron conceptualizados por primera vez por Gerolamo Cardano en el siglo XVI. Python, entre otros lenguajes de programación, trata a los números complejos como ciudadanos de primera clase. Esto significa que están integrados en el lenguaje, con características fáciles de usar, evitando la necesidad de importar bibliotecas externas para operaciones básicas.

Sin embargo, para cálculos numéricos intensivos, Python tiene una biblioteca llamada `cmath`, que está específicamente destinada a números complejos. Tiene funciones adicionales como `exp`, `log` y operaciones trigonométricas.

Cuando Python no es suficiente, podrías recurrir a bibliotecas como NumPy, especialmente para operaciones de matrices que involucran números complejos. NumPy proporciona operaciones optimizadas y vectorizadas que son cruciales para el rendimiento en computación numérica.

## Ver También
Consulta estos recursos para aprender más:

- Documentación oficial de Python sobre números complejos: https://docs.python.org/3/library/stdtypes.html#typesnumeric
- Documentación del módulo `cmath`: https://docs.python.org/3/library/cmath.html
- NumPy para manejar matrices de números complejos: https://numpy.org/doc/stable/user/absolute_beginners.html#the-basics
