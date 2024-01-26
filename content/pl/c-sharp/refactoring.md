---
title:                "Refaktoryzacja"
date:                  2024-01-26T01:17:24.119447-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktoryzacja"
programming_language: "C#"
category:             "C#"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/refactoring.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Refaktoryzacja to proces restrukturyzacji istniejącego kodu komputerowego bez zmiany jego zewnętrznego zachowania. Programiści robią to, aby uporządkować kod, zwiększyć czytelność, zmniejszyć złożoność i poprawić możliwość utrzymania kodu.

## Jak to zrobić:

Prześledźmy refaktoryzację prostej metody C#, która oblicza i wyświetla sumę tablicy liczb:

Przed refaktoryzacją:
```C#
public class Calculator
{
    public void CalculateSum()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };
        int sum = 0;
        for (int i = 0; i < numbers.Length; i++)
        {
            sum += numbers[i];
        }
        Console.WriteLine("The sum is " + sum);
    }
}
```

Po refaktoryzacji:
```C#
public class Calculator
{
    private readonly int[] _numbers;

    public Calculator(int[] numbers)
    {
        _numbers = numbers;
    }

    public int CalculateSum()
    {
        return _numbers.Sum();
    }

    public void DisplaySum()
    {
        Console.WriteLine($"The sum is {CalculateSum()}");
    }
}

// Użycie:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

Dzięki refaktoryzacji oddzieliliśmy obowiązki, uczyniliśmy klasę `Calculator` bardziej elastyczną, umożliwiając jej przyjmowanie dowolnej tablicy liczb oraz wykorzystaliśmy LINQ do bardziej zwięzłego wykonania obliczeń sumy.

## Wnikliwe spojrzenie

Refaktoryzacja ma swoje korzenie w społeczności programistów Smalltalk i została spopularyzowana w latach 90. przez książkę Martina Fowlera "Refaktoryzacja: ulepszanie struktury istniejącego kodu". Na przestrzeni lat stała się fundamentalną częścią metodyk zwinnych i dobrych praktyk kodowania.

Istnieje wiele podejść do refaktoryzacji, takich jak Red-Green-Refactor w rozwoju sterowanym testami (TDD). Zapewnia to, że refaktoryzacja nie wprowadza błędów, rozpoczynając od nieudanego testu, sprawiając, że przejdzie, a następnie porządkując kod.

Podczas wdrażania refaktoryzacji niezwykle ważne jest posiadanie wszechstronnego zestawu testów, aby upewnić się, że żadna funkcjonalność nie zostanie zaburzona w trakcie procesu. Narzędzia do automatycznej refaktoryzacji, takie jak ReSharper dla C#, mogą również pomóc w tym procesie, oferując bezpieczne sposoby na zmianę struktur kodu. Niemniej jednak, narzędzia powinny być uzupełnieniem głębokiego zrozumienia bazy kodu i zasad kodowania.

## Zobacz również

- Klasyczna praca Martina Fowlera o Refaktoryzacji: [Refaktoryzacja: ulepszanie struktury istniejącego kodu](https://martinfowler.com/books/refactoring.html)
- Przewodnik Microsoftu dotyczący refaktoryzacji w Visual Studio: [Refaktoryzacja (C#)](https://docs.microsoft.com/pl-pl/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- Szczegółowe spojrzenie na wzorce refaktoryzacji z przykładami: [SourceMaking Refaktoryzacja](https://sourcemaking.com/refactoring)