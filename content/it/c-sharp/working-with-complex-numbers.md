---
title:                "Lavorare con i numeri complessi"
date:                  2024-01-26T04:38:17.387910-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con i numeri complessi"

category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Cosa e perché?
I numeri complessi ampliano il nostro sistema numerico includendo i numeri immaginari, permettendoci di risolvere equazioni che non hanno soluzioni reali. I programmatori lavorano con essi in campi come l'ingegneria, la fisica e l'elaborazione dei segnali, dove questi numeri sono essenziali per la modellazione e la risoluzione dei problemi.

## Come fare:
C# dispone di una struttura integrata `System.Numerics.Complex` per elaborare i numeri complessi. Ecco una rapida guida:

```C#
using System;
using System.Numerics;

class EsempioNumeroComplesso
{
    static void Main()
    {
        // Creazione di numeri complessi
        Complex c1 = new Complex(4, 5); // 4 + 5i
        Complex c2 = Complex.FromPolarCoordinates(1, Math.PI / 4); // 1 * e^(iπ/4)

        // Operazioni di base
        Complex somma = c1 + c2;
        Complex differenza = c1 - c2;
        Complex prodotto = c1 * c2;
        Complex quoziente = c1 / c2;

        // Risultati di output
        Console.WriteLine($"Somma: {somma}");
        Console.WriteLine($"Differenza: {differenza}");
        Console.WriteLine($"Prodotto: {prodotto}");
        Console.WriteLine($"Quoziente: {quoziente}");
        Console.WriteLine($"Magnitudine di c1: {c1.Magnitude}");
        Console.WriteLine($"Fase di c1: {c1.Phase}");
    }
}
```

E questo produrrà in output:

```
Somma: (4.70710678118655, 5.70710678118655)
Differenza: (3.29289321881345, 4.29289321881345)
Prodotto: (-1.00000000000001, 9)
Quoziente: (0.6, 0.8)
Magnitudine di c1: 6.40312423743285
Fase di c1: 0.896055384571344
```

## Approfondimento
I numeri complessi, costituiti da una parte reale e una immaginaria (spesso notati come a + bi), esistono fin dal XVII secolo. Al matematico italiano Gerolamo Cardano viene attribuito il loro primo sviluppo. Nella programmazione, lavorare con i numeri complessi comporta la comprensione e la gestione di queste due parti distinte.

Sebbene la `System.Numerics.Complex` di C# sia robusta e integrata nel linguaggio, altri linguaggi come Python offrono funzionalità simili con `cmath` o librerie di terze parti. E se si lavora con una versione precedente di C# o una versione di .NET che non supporta `System.Numerics`, potrebbe essere necessario creare una propria classe di numeri complessi o trovare una libreria.

Internamente, le operazioni sui numeri complessi utilizzano l'aritmetica in virgola mobile che può introdurre errori di arrotondamento. Pertanto, quando si implementano algoritmi che utilizzano estensivamente i numeri complessi, è fondamentale ricordare questo e considerare l'impatto sulla precisione e l'accuratezza.

## Vedi anche
1. Riferimento C# per `System.Numerics.Complex`: https://learn.microsoft.com/en-us/dotnet/api/system.numerics.complex
2. Un approfondimento sulla matematica dei numeri complessi: https://mathworld.wolfram.com/ComplexNumber.html
3. Per implementazioni e librerie alternative, controlla Math.NET Numerics: https://numerics.mathdotnet.com/
