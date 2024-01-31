---
title:                "Redondeo de números"
date:                  2024-01-26T03:43:32.278305-07:00
model:                 gpt-4-0125-preview
simple_title:         "Redondeo de números"

category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/rounding-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Redondear números significa ajustarlos al lugar de valor especificado más cercano—piensa en simplificarlos a una forma más simple. Los programadores redondean para controlar la precisión, mejorar el rendimiento o cuando muestran resultados amigables al usuario—como precios que no necesitan tres lugares decimales.

## Cómo:
Aquí está el boleto de ida y vuelta para redondear números en C#:

```csharp
using System;

public class EjemplosDeRedondeo
{
    public static void Main()
    {
        double numeroOriginal = 123.4567;

        // Redondear al entero más cercano
        double redondeado = Math.Round(numeroOriginal);
        Console.WriteLine(redondeado); // Salida: 123

        // Especificar número de lugares decimales
        double redondeadoDosDecimales = Math.Round(numeroOriginal, 2);
        Console.WriteLine(redondeadoDosDecimales); // Salida: 123.46

        // Redondear hacia arriba independientemente del siguiente dígito
        double redondeadoArriba = Math.Ceiling(numeroOriginal);
        Console.WriteLine(redondeadoArriba); // Salida: 124

        // Redondear hacia abajo independientemente del siguiente dígito
        double redondeadoAbajo = Math.Floor(numeroOriginal);
        Console.WriteLine(redondeadoAbajo); // Salida: 123
    }
}
```

## Análisis Profundo
En el pasado, redondear era pan comido para recortar costos computacionales. Cada ciclo contaba, y recortar números ahorraba tiempo precioso. Avanzando rápidamente al C# moderno, se trata de manejar la notoria predisposición de los dobles y decimales a errores de precisión y peculiaridades de visualización.

Más allá de `Math.Round`, `Math.Floor` y `Math.Ceiling`, el enumerado `MidpointRounding` nos permite dictar el destino de los pobres dígitos que se sientan en medio—es el cruce de caminos entre las reglas bancarias y la justicia del juego de "redondear a la mitad hacia arriba".

Para audiencias más difíciles, como aplicaciones serias de matemáticas o finanzas, tenemos `decimal` sobre `double`, reduciendo el drama del redondeo al ofrecer una mayor precisión—menos redondeo, menos problemas.

## Ver También
- [Documentación oficial de C# sobre `Math.Round`](https://docs.microsoft.com/en-us/dotnet/api/system.math.round)
- [Stack Overflow: ¿Cuándo debería usar Double en lugar de Decimal?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [Estándar IEEE para Aritmética de Punto Flotante (IEEE 754)](https://es.wikipedia.org/wiki/IEEE_coma_flotante)
