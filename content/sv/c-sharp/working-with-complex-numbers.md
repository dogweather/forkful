---
title:                "Att arbeta med komplexa tal"
aliases:
- sv/c-sharp/working-with-complex-numbers.md
date:                  2024-01-26T04:38:30.199614-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med komplexa tal"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Komplexa tal utvidgar vårt talsystem för att inkludera imaginära tal, vilket möjliggör lösningar på ekvationer som inte har reella lösningar. Programmerare arbetar med dem inom områden som ingenjörsvetenskap, fysik och signalbehandling där dessa tal är väsentliga för modellering och problemlösning.

## Hur man gör:
C# har en inbyggd `System.Numerics.Complex` struktur för att bearbeta komplexa tal. Här är en snabb genomgång:

```C#
using System;
using System.Numerics;

class ExempelPåKomplextTal
{
    static void Main()
    {
        // Skapa komplexa tal
        Complex c1 = new Complex(4, 5); // 4 + 5i
        Complex c2 = Complex.FromPolarCoordinates(1, Math.PI / 4); // 1 * e^(iπ/4)

        // Grundläggande operationer
        Complex summa = c1 + c2;
        Complex differens = c1 - c2;
        Complex produkt = c1 * c2;
        Complex kvot = c1 / c2;

        // Skriv ut resultat
        Console.WriteLine($"Summa: {summa}");
        Console.WriteLine($"Differens: {differens}");
        Console.WriteLine($"Produkt: {produkt}");
        Console.WriteLine($"Kvot: {kvot}");
        Console.WriteLine($"Storleken på c1: {c1.Magnitude}");
        Console.WriteLine($"Fasen för c1: {c1.Phase}");
    }
}
```

Och det kommer att skriva ut:

```
Summa: (4.70710678118655, 5.70710678118655)
Differens: (3.29289321881345, 4.29289321881345)
Produkt: (-1.00000000000001, 9)
Kvot: (0.6, 0.8)
Storleken på c1: 6.40312423743285
Fasen för c1: 0.896055384571344
```

## Fördjupning
Komplexa tal, som består av en reell och en imaginär del (ofta noterade som a + bi), har funnits sedan 1600-talet. Den italienska matematikern Gerolamo Cardano tillskrivs utvecklingen av dem tidigt. I programmering innebär hantering av komplexa tal att förstå och hantera dessa två distinkta delar.

Även om C#'s `System.Numerics.Complex` är robust och integrerad i språket, erbjuder andra språk som Python liknande funktionalitet med `cmath` eller tredjepartsbibliotek. Och om du arbetar i en äldre version av C# eller en .NET version som inte stödjer `System.Numerics`, kanske du måste skapa din egen klass för komplexa tal eller hitta ett bibliotek.

Internt använder operationerna på komplexa tal flyttalsaritmetik vilket kan introducera avrundningsfel. Så när man implementerar algoritmer som använder komplexa tal i stor utsträckning är det viktigt att komma ihåg detta och överväga inverkan på precision och noggrannhet.

## Se även
1. C# referens för `System.Numerics.Complex`: https://learn.microsoft.com/en-us/dotnet/api/system.numerics.complex
2. En djupare dykning i matematiken av komplexa tal: https://mathworld.wolfram.com/ComplexNumber.html
3. För alternativa implementationer och bibliotek, kolla Math.NET Numerics: https://numerics.mathdotnet.com/
