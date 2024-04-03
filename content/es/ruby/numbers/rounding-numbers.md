---
date: 2024-01-26 03:46:45.605535-07:00
description: "C\xF3mo hacerlo: ."
lastmod: '2024-03-13T22:44:59.584652-06:00'
model: gpt-4-0125-preview
summary: .
title: "Redondeo de n\xFAmeros"
weight: 13
---

## Cómo hacerlo:
```Ruby
# Redondeo básico
puts 3.14159.round      # => 3
puts 2.6.round          # => 3

# Especificando precisión
puts 3.14159.round(2)   # => 3.14
puts 2.675.round(2)     # => 2.68

# Redondeo hacia abajo
puts 2.9.floor          # => 2

# Redondeo hacia arriba
puts 2.1.ceil           # => 3

# Redondeo hacia cero
puts -2.9.round         # => -3
puts -2.9.truncate      # => -2
```

Salida de muestra:
```
3
3
3.14
2.68
2
3
-3
-2
```

## Análisis Profundo
Redondear números no es algo nuevo—los humanos lo han estado haciendo por siglos para facilitar los cálculos o para trabajar dentro de los límites de sus herramientas. En Ruby, el método `round` es versátil, con la capacidad de redondear al número entero más cercano por defecto o a un lugar decimal especificado.

Una alternativa a `round` es `floor` para redondear siempre hacia abajo, y `ceil` para redondear siempre hacia arriba, independientemente del valor del número. Para simplemente cortar los lugares decimales, tienes `truncate`.

Históricamente, cuando se trata de computadoras, el redondeo se vuelve crítico al tratar con aritmética de punto flotante debido a su imprecisión inherente. Ruby, como la mayoría de los lenguajes, sigue el estándar IEEE 754 para los números de punto flotante, lo que significa que maneja el redondeo de una manera que la mayoría de los programadores deberían poder predecir y confiar.

Sin embargo, hay más en esto—cosas como el redondeo del banquero (también conocido como redondeo a la mitad par) son conceptos que los desarrolladores de Ruby pueden necesitar implementar manualmente, ya que el método `round` no lo ofrece directamente.

## Véase También
- La [Documentación de Ruby](https://ruby-doc.org/core-3.0.0/Float.html#method-i-round) para el método `round` de los Floats.
- [Estándar IEEE para la Aritmética de Punto Flotante (IEEE 754)](https://ieeexplore.ieee.org/document/4610935).
- [Comprendiendo la Precisión de Punto Flotante](https://floating-point-gui.de/), para una comprensión más profunda de cómo las computadoras manejan los números decimales.
