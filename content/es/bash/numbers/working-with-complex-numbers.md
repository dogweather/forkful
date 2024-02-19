---
aliases:
- /es/bash/working-with-complex-numbers/
date: 2024-01-26 04:36:43.925360-07:00
description: "Los n\xFAmeros complejos consisten en una parte real y una parte imaginaria.\
  \ Los programadores los usan en campos como el procesamiento de se\xF1ales, la\u2026"
lastmod: 2024-02-18 23:09:10.158614
model: gpt-4-0125-preview
summary: "Los n\xFAmeros complejos consisten en una parte real y una parte imaginaria.\
  \ Los programadores los usan en campos como el procesamiento de se\xF1ales, la\u2026"
title: "Trabajando con n\xFAmeros complejos"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Los números complejos consisten en una parte real y una parte imaginaria. Los programadores los usan en campos como el procesamiento de señales, la mecánica cuántica y siempre que el cálculo los requiera, porque los números reales normales simplemente no son suficientes.

## Cómo hacerlo:
Bash no soporta números complejos de manera nativa. A menudo se utiliza una herramienta externa como `bc` con su opción `-l`. Aquí está cómo manipular números complejos en bash:

```bash
echo "sqrt(-1)" | bc -l
```

Salida:
```bash
j
```

Multiplicación:

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

Salida:
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## Análisis Profundo
Los números complejos existen desde el siglo XVI, pero los lenguajes de scripting como Bash no están preparados para cálculos matemáticos como los números complejos directamente. Es por eso que `bc` u otras herramientas como `awk` a menudo entran en juego. Algunos lenguajes alternativos para trabajar con números complejos son Python con su módulo `cmath` y MATLAB, que están construidos para funciones matemáticas más avanzadas. En cuanto a Bash, todo se trata de aprovechar herramientas - `bc` usa la 'i' minúscula para representar la unidad imaginaria y admite operaciones básicas como suma, resta, multiplicación y división.

## Ver También
- El manual de `bc`: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave (alternativa para MATLAB): https://www.gnu.org/software/octave/
- Módulo `cmath` de Python: https://docs.python.org/3/library/cmath.html
