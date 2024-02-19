---
aliases:
- /pl/c-sharp/rounding-numbers/
date: 2024-01-26 03:43:26.326395-07:00
description: "Zaokr\u0105glanie liczb oznacza dostosowanie ich do najbli\u017Cszej\
  \ okre\u015Blonej warto\u015Bci miejsca \u2013 pomy\u015Bl o sprowadzeniu ich do\
  \ prostszej formy. Programi\u015Bci\u2026"
lastmod: 2024-02-18 23:08:49.605641
model: gpt-4-0125-preview
summary: "Zaokr\u0105glanie liczb oznacza dostosowanie ich do najbli\u017Cszej okre\u015B\
  lonej warto\u015Bci miejsca \u2013 pomy\u015Bl o sprowadzeniu ich do prostszej formy.\
  \ Programi\u015Bci\u2026"
title: "Zaokr\u0105glanie liczb"
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
