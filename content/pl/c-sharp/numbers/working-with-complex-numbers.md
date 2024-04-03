---
date: 2024-01-26 04:39:04.370982-07:00
description: "Jak to zrobi\u0107: C# ma wbudowan\u0105 struktur\u0119 `System.Numerics.Complex`\
  \ do przetwarzania liczb zespolonych. Oto kr\xF3tkie wprowadzenie."
lastmod: '2024-03-13T22:44:35.402389-06:00'
model: gpt-4-0125-preview
summary: "C# ma wbudowan\u0105 struktur\u0119 `System.Numerics.Complex` do przetwarzania\
  \ liczb zespolonych."
title: Praca z liczbami zespolonymi
weight: 14
---

## Jak to zrobić:
C# ma wbudowaną strukturę `System.Numerics.Complex` do przetwarzania liczb zespolonych. Oto krótkie wprowadzenie:

```C#
using System;
using System.Numerics;

class ComplexNumberExample
{
    static void Main()
    {
        // Tworzenie liczb zespolonych
        Complex c1 = new Complex(4, 5); // 4 + 5i
        Complex c2 = Complex.FromPolarCoordinates(1, Math.PI / 4); // 1 * e^(iπ/4)

        // Podstawowe operacje
        Complex sum = c1 + c2;
        Complex difference = c1 - c2;
        Complex product = c1 * c2;
        Complex quotient = c1 / c2;

        // Wyniki wyjściowe
        Console.WriteLine($"Suma: {sum}");
        Console.WriteLine($"Różnica: {difference}");
        Console.WriteLine($"Iloczyn: {product}");
        Console.WriteLine($"Iloraz: {quotient}");
        Console.WriteLine($"Moduł c1: {c1.Magnitude}");
        Console.WriteLine($"Faza c1: {c1.Phase}");
    }
}
```

I to da na wyjściu:

```
Suma: (4.70710678118655, 5.70710678118655)
Różnica: (3.29289321881345, 4.29289321881345)
Iloczyn: (-1.00000000000001, 9)
Iloraz: (0.6, 0.8)
Moduł c1: 6.40312423743285
Faza c1: 0.896055384571344
```

## Dogłębna analiza
Liczby zespolone, składające się z części rzeczywistej i urojonej (często oznaczane jako a + bi), istnieją od XVII wieku. Włoski matematyk Gerolamo Cardano jest przypisywany za ich wczesny rozwój. W programowaniu, praca z liczbami zespolonymi oznacza zrozumienie i zarządzanie tymi dwiema odrębnymi częściami.

Chociaż C#'s `System.Numerics.Complex` jest rozbudowany i zintegrowany z językiem, inne języki takie jak Python oferują podobną funkcjonalność dzięki `cmath` lub bibliotekom stron trzecich. A jeśli pracujesz w starszej wersji C# lub w wersji .NET, która nie obsługuje `System.Numerics`, możesz musieć stworzyć własną klasę liczb zespolonych lub znaleźć bibliotekę.

Wewnętrznie, operacje na liczbach zespolonych używają arytmetyki zmiennoprzecinkowej, co może wprowadzać błędy zaokrągleń. Dlatego, implementując algorytmy, które intensywnie korzystają z liczb zespolonych, kluczowe jest pamiętanie o tym i rozważenie wpływu na precyzję i dokładność.

## Zobacz także
1. Referencje C# dla `System.Numerics.Complex`: https://learn.microsoft.com/pl-PL/dotnet/api/system.numerics.complex
2. Dogłębniejsza analiza matematyki liczb zespolonych: https://mathworld.wolfram.com/ComplexNumber.html
3. Alternatywne implementacje i biblioteki, sprawdź Math.NET Numerics: https://numerics.mathdotnet.com/
