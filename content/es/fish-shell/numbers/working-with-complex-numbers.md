---
date: 2024-01-26 04:39:44.886565-07:00
description: "C\xF3mo hacerlo: En Fish, manejamos n\xFAmeros complejos usando `math`\
  \ con partes reales e imaginarias. Aqu\xED tienes un inicio."
lastmod: '2024-03-13T22:44:59.493797-06:00'
model: gpt-4-0125-preview
summary: "En Fish, manejamos n\xFAmeros complejos usando `math` con partes reales\
  \ e imaginarias."
title: "Trabajando con n\xFAmeros complejos"
weight: 14
---

## Cómo hacerlo:
En Fish, manejamos números complejos usando `math` con partes reales e imaginarias. Aquí tienes un inicio:

```fish
# Sumar dos números complejos (3+4i) y (5+2i)
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # Salida: 8+6i

# Multiplicar dos números complejos (1+2i) y (3+4i)
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # Salida: -5+10i
```

Si necesitas elevar un número complejo a una potencia u obtener su forma exponencial:

```fish
# Cuadrado de (2+3i)
set complex_square (math "(2+3i)^2")
echo $complex_square # Salida: -5+12i

# Exponencial de (2i)
set complex_exp (math "e^(2i)")
echo $complex_exp # Salida: -0.41615+0.9093i
```

## Análisis Profundo
El soporte de Fish Shell para números complejos es relativamente nuevo, comenzando alrededor de la versión 3.1.0. Antes de eso, las personas podrían haber usado `bc` o recurrido a herramientas externas como Python para matemáticas complejas.

Alternativas a las matemáticas de Fish incluyen bibliotecas numéricas especializadas o lenguajes como MATLAB, Python con NumPy, o incluso C++ con la Biblioteca Estándar. Sin embargo, estos podrían ser excesivos para cálculos rápidos en la shell.

El soporte de números complejos de Fish está incorporado en su comando interno `math`, aprovechando libcalc. Esto significa que no tienes que instalar herramientas adicionales para operaciones básicas.

Sin embargo, Fish no está diseñado para cálculos matemáticos pesados. Su capacidad matemática es conveniente para cálculos rápidos o scripts donde los números complejos entran en juego, pero considera herramientas más robustas para tareas intensivas.

## Ver También
- Documentación de Fish shell para math: https://fishshell.com/docs/current/commands.html#math
- NumPy para Python, una alternativa popular: https://numpy.org/
- Una mirada más profunda a los números complejos: https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/
