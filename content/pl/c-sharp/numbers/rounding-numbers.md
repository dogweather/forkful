---
title:                "Zaokrąglanie liczb"
aliases:
- /pl/c-sharp/rounding-numbers/
date:                  2024-01-26T03:43:26.326395-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zaokrąglanie liczb"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/rounding-numbers.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Zaokrąglanie liczb oznacza dostosowanie ich do najbliższej określonej wartości miejsca – pomyśl o sprowadzeniu ich do prostszej formy. Programiści zaokrąglają liczby, aby kontrolować precyzję, zwiększać wydajność lub kiedy pokazują wyniki przyjazne dla użytkownika – jak ceny, które nie potrzebują trzech miejsc po przecinku.

## Jak to zrobić:
Oto bilet w obie strony na zaokrąglanie liczb w C#:

```csharp
using System;

public class RoundingExamples
{
    public static void Main()
    {
        double originalNumber = 123.4567;

        // Zaokrąglij do najbliższej liczby całkowitej
        double rounded = Math.Round(originalNumber);
        Console.WriteLine(rounded); // Wynik: 123

        // Określ liczbę miejsc po przecinku
        double roundedTwoDecimalPlaces = Math.Round(originalNumber, 2);
        Console.WriteLine(roundedTwoDecimalPlaces); // Wynik: 123.46

        // Zaokrąglij w górę niezależnie od następnej cyfry
        double roundedUp = Math.Ceiling(originalNumber);
        Console.WriteLine(roundedUp); // Wynik: 124

        // Zaokrąglij w dół niezależnie od następnej cyfry
        double roundedDown = Math.Floor(originalNumber);
        Console.WriteLine(roundedDown); // Wynik: 123
    }
}
```

## Szczegółowe omówienie
Kiedyś zaokrąglanie było oczywistym sposobem na zmniejszenie kosztów obliczeniowych. Każdy cykl się liczył, a przycinanie liczb oszczędzało cenny czas. Przeskakując do nowoczesnego C#, chodzi o zarządzanie notoryczną skłonnością typów double i decimal do błędów precyzji i dziwactw wyświetlania.

Poza `Math.Round`, `Math.Floor` i `Math.Ceiling`, wyliczenie `MidpointRounding` pozwala nam decydować o losie biednych, siedzących na środku cyfr – to rozdroże między regułami bankowymi a uczciwością placu zabaw "zaokrąglaj połowy w górę".

Dla trudniejszych przypadków, jak poważne aplikacje matematyczne czy finansowe, mamy `decimal` zamiast `double`, redukując dramat zaokrąglania poprzez oferowanie wyższej precyzji – mniej zaokrągleń, mniej problemów.

## Zobacz także
- [Oficjalna dokumentacja C# o `Math.Round`](https://docs.microsoft.com/en-us/dotnet/api/system.math.round)
- [Stack Overflow: Kiedy powinienem używać Double zamiast Decimal?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [Standard IEEE dla arytmetyki zmiennoprzecinkowej (IEEE 754)](https://en.wikipedia.org/wiki/IEEE_754)
