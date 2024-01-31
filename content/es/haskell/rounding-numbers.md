---
title:                "Redondeo de números"
date:                  2024-01-26T03:44:36.786823-07:00
model:                 gpt-4-0125-preview
simple_title:         "Redondeo de números"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/rounding-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Redondear números significa ajustarlos al entero más cercano o a un lugar decimal especificado. Los programadores redondean números para controlar la precisión, personalizar salidas para la presentación al usuario o reducir los costos de computación para operaciones con punto flotante.

## Cómo:

Haskell utiliza las funciones `round`, `ceiling`, `floor` y `truncate` de `Prelude` para operaciones de redondeo.

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- Redondear a un lugar decimal específico no está en Prelude.
  -- Aquí hay una función personalizada:
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## Estudio Profundo

Históricamente, el redondeo es significativo en análisis numérico y ciencias de la computación porque es crucial para minimizar la acumulación de errores en cálculos, particularmente antes de que las representaciones de punto flotante fueran estandarizadas con IEEE 754.

¿A qué redondear? `round` te lleva al entero más cercano, hacia arriba o hacia abajo. `ceiling` y `floor` siempre redondean hacia arriba o hacia abajo al entero más cercano, respectivamente, mientras que `truncate` simplemente elimina los puntos decimales.

Alternativas a estas funciones podrían involucrar lógica personalizada, como nuestro `roundTo`, o podrías utilizar librerías (como Data.Fixed) para requisitos más complejos.

Ten cuidado con los resultados inesperados debido a cómo Haskell maneja los casos intermedios en `round` (redondea al número par más cercano).

## Ver También

- Documentación de Haskell Prelude para funciones de redondeo: https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- Wiki de Haskell sobre aritmética de punto flotante: https://wiki.haskell.org/Floating_point_arithmetic
- Estándar IEEE 754-2008 para más información sobre cómo se maneja el punto flotante en muchos idiomas: https://ieeexplore.ieee.org/document/4610935
