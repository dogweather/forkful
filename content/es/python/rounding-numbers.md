---
title:                "Redondeo de números"
aliases:
- es/python/rounding-numbers.md
date:                  2024-01-26T03:46:17.495175-07:00
model:                 gpt-4-0125-preview
simple_title:         "Redondeo de números"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/rounding-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Redondear números significa ajustarlos para que estén más cerca de un valor más simple o más significativo. Los programadores redondean los números para simplificar resultados, limitar lugares decimales para la visualización o para ciertos fines matemáticos.

## Cómo hacerlo:
Aquí tienes los detalles sobre cómo redondear números en Python:

```python
# Redondear un número al entero más cercano
print(round(8.67))  # Salida: 9

# Redondear un número a un número especificado de lugares decimales
print(round(8.67, 1))  # Salida: 8.7

# Los números pares se redondean hacia abajo y los impares hacia arriba cuando están equidistantes
print(round(2.5))  # Salida: 2
print(round(3.5))  # Salida: 4
```

## Análisis Profundo
En Python, `round()` no se limita a eliminar decimales. Históricamente, Python, como muchos otros lenguajes, sigue el "redondeo a la mitad hacia el par más cercano" o "redondeo del banquero". Esto minimiza el error acumulativo en sumas o promedios, lo que importa en cálculos financieros.

Para alternativas, tienes `math.floor()` y `math.ceil()` del módulo math de Python, arrastrando números hacia abajo o hacia arriba al siguiente número entero. Pero si lo que buscas es precisión, `quantize()` del módulo `decimal` te permite especificar el comportamiento de redondeo.

Bajo el capó, `round()` maneja números binarios de punto flotante. Dado que algunos decimales no se pueden expresar exactamente en binario, podrías obtener sorpresas con cosas como `round(2.675, 2)` que no se convierte en `2.68` como se esperaba. Aquí entran `decimal` o `fractions` para alta precisión.

## Ver También
- Documentación de Python sobre funciones integradas: https://docs.python.org/3/library/functions.html#round
- Aritmética de punto fijo y flotante decimal: https://docs.python.org/3/library/decimal.html
- Módulo math de Python: https://docs.python.org/3/library/math.html
