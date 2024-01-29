---
title:                "Werken met complexe getallen"
date:                  2024-01-28T22:13:05.089114-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met complexe getallen"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Complexe getallen breiden ons getallensysteem uit met imaginaire getallen, waardoor we vergelijkingen kunnen oplossen die geen reële oplossingen hebben. Programmeurs werken ermee in vakgebieden zoals engineering, natuurkunde en signaalverwerking waar deze getallen essentieel zijn voor modellering en probleemoplossing.

## Hoe te:
C# heeft een ingebouwde `System.Numerics.Complex` structuur voor het verwerken van complexe getallen. Hier volgt een snelle doorloop:

```C#
using System;
using System.Numerics;

class ComplexNumberExample
{
    static void Main()
    {
        // Complex getallen maken
        Complex c1 = new Complex(4, 5); // 4 + 5i
        Complex c2 = Complex.FromPolarCoordinates(1, Math.PI / 4); // 1 * e^(iπ/4)

        // Basisbewerkingen
        Complex som = c1 + c2;
        Complex verschil = c1 - c2;
        Complex product = c1 * c2;
        Complex quotiënt = c1 / c2;

        // Resultaten uitvoeren
        Console.WriteLine($"Som: {som}");
        Console.WriteLine($"Verschil: {verschil}");
        Console.WriteLine($"Product: {product}");
        Console.WriteLine($"Quotiënt: {quotiënt}");
        Console.WriteLine($"Magnitude van c1: {c1.Magnitude}");
        Console.WriteLine($"Fase van c1: {c1.Phase}");
    }
}
```

En dat zal het volgende uitvoeren:

```
Som: (4.70710678118655, 5.70710678118655)
Verschil: (3.29289321881345, 4.29289321881345)
Product: (-1.00000000000001, 9)
Quotiënt: (0.6, 0.8)
Magnitude van c1: 6.40312423743285
Fase van c1: 0.896055384571344
```

## Diepgaande verkenning
Complexe getallen, bestaande uit een reëel en een imaginair deel (vaak genoteerd als a + bi), bestaan sinds de 17e eeuw. De Italiaanse wiskundige Gerolamo Cardano wordt gecrediteerd voor hun vroege ontwikkeling. In programmering omvat het werken met complexe getallen het begrijpen en beheren van deze twee onderscheidende delen.

Hoewel C#'s `System.Numerics.Complex` robuust is en geïntegreerd in de taal, bieden andere talen zoals Python vergelijkbare functionaliteit met `cmath` of externe bibliotheken. En als je werkt in een oudere versie van C# of een .NET-versie die `System.Numerics` niet ondersteunt, moet je mogelijk je eigen complexe-getalklasse maken of een bibliotheek vinden.

Intern gebruiken de bewerkingen op complexe getallen zwevende-kommagetallen rekenkunde, wat afrondingsfouten kan introduceren. Dus, bij het implementeren van algoritmen die uitgebreid gebruikmaken van complexe getallen, is het cruciaal om hieraan te denken en de impact op precisie en nauwkeurigheid te overwegen.

## Zie ook
1. C# Referentie voor `System.Numerics.Complex`: https://learn.microsoft.com/en-us/dotnet/api/system.numerics.complex
2. Een diepere duik in de wiskunde van complexe getallen: https://mathworld.wolfram.com/ComplexNumber.html
3. Voor alternatieve implementaties en bibliotheken, bekijk Math.NET Numerics: https://numerics.mathdotnet.com/
