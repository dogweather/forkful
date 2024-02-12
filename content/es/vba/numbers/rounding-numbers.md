---
title:                "Redondeo de números"
aliases:
- /es/vba/rounding-numbers/
date:                  2024-02-01T22:00:57.673812-07:00
model:                 gpt-4-0125-preview
simple_title:         "Redondeo de números"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/vba/rounding-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?

Redondear números en la programación se refiere a aproximar un número a su número entero más cercano o a un cierto número de lugares decimales. Los programadores redondean números para simplificar cifras, mejorar la legibilidad o cumplir con criterios numéricos específicos en cálculos, especialmente en cálculos financieros donde la precisión importa.

## Cómo hacerlo:

En Visual Basic para Aplicaciones (VBA), el redondeo se puede lograr utilizando varias funciones, cada una adecuada para escenarios específicos. Aquí están las funciones más comúnmente usadas con ejemplos:

1. **Función Round**:
   La función `Round` redondea un número a un número especificado de dígitos.
   ```basic
   Dim roundedNumber As Double
   roundedNumber = Round(3.14159, 2)  ' Salida: 3.14
   MsgBox roundedNumber
   ```
   
2. **Funciones Int y Fix**:
   Tanto las funciones `Int` como `Fix` se usan para redondear números hacia abajo al entero más cercano, pero se comportan de manera diferente con números negativos.
   ```basic
   Dim intRounded As Integer
   Dim fixRounded As Integer
   
   intRounded = Int(-3.14159)  ' Salida: -4
   fixRounded = Fix(-3.14159)  ' Salida: -3
   
   MsgBox "Int: " & intRounded & ", Fix: " & fixRounded
   ```

3. **Funciones Ceiling y Floor**:
   VBA carece de las funciones integradas `Ceiling` y `Floor` encontradas en otros lenguajes. Para simular esto, usa `Application.WorksheetFunction.Ceiling_Math` y `Application.WorksheetFunction.Floor_Math` para Excel VBA.
   ```basic
   Dim ceilingNumber As Double
   Dim floorNumber As Double
   
   ceilingNumber = Application.WorksheetFunction.Ceiling_Math(3.14159)  ' Salida: 4
   floorNumber = Application.WorksheetFunction.Floor_Math(3.14159)  ' Salida: 3
   
   MsgBox "Ceiling: " & ceilingNumber & ", Floor: " & floorNumber
   ```

## Análisis Profundo

La función `Round` en VBA es fundamentalmente diferente de los métodos de redondeo en otros lenguajes debido a su uso del **Redondeo del Banquero**. El Redondeo del Banquero redondea al número par más cercano cuando está exactamente a la mitad entre dos números, reduciendo el sesgo en los cálculos a lo largo de un gran conjunto de datos y proporcionando un resultado más estadísticamente significativo. Sin embargo, esto puede llevar a un comportamiento inesperado para aquellos no familiarizados con él, especialmente cuando se espera precisión integral en cada caso.

En contraste, muchos lenguajes de programación y sistemas utilizan el "redondeo aritmético" o "redondeo hacia arriba", donde un número exactamente a la mitad entre dos valores posibles de redondeo siempre se redondea hacia arriba. Al traducir o portar código de otros lenguajes a VBA, los programadores deben tener en cuenta estas diferencias para evitar sutiles errores o inexactitudes en aplicaciones financieras y estadísticas.

Aunque VBA ofrece una variedad de funciones para el redondeo, la ausencia de funciones `Ceiling` y `Floor` (sin recurrir a WorksheetFunction de Excel) destaca una limitación en sus capacidades nativas. Los programadores provenientes de lenguajes más ricos en características podrían encontrar estas omisiones inconvenientes y podrían necesitar implementar soluciones personalizadas o adaptar sus cálculos para usar las funciones disponibles. A pesar de estas limitaciones, entender y usar correctamente las funciones de redondeo de VBA puede ayudar a asegurar que los cálculos numéricos sean tanto precisos como cumplan con los requisitos de la mayoría de las aplicaciones.
