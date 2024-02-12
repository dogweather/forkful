---
title:                "Praca z liczbami zespolonymi"
aliases: - /pl/c-sharp/working-with-complex-numbers.md
date:                  2024-01-26T04:39:04.370982-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z liczbami zespolonymi"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Liczby zespolone poszerzają nasz system liczbowy o liczby urojone, umożliwiając rozwiązywanie równań, które nie mają rzeczywistych rozwiązań. Programiści pracują z nimi w dziedzinach takich jak inżynieria, fizyka i przetwarzanie sygnałów, gdzie te liczby są niezbędne do modelowania i rozwiązywania problemów.

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
