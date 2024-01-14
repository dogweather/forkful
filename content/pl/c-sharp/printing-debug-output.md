---
title:    "C#: Drukowanie wyjścia debugowania"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Często przeprowadzam debugowanie i kolejny raz muszę to robić, bo zapomniałem dodać kilku wypisów debugujących. Dlatego postanowiłem napisać artykuł, aby wyjaśnić czym jest wypisywanie debugujące oraz dlaczego jest to ważne.

## Jak To Zrobić

Aby wypisywać debug w C#, musisz użyć metody `Console.WriteLine()`, która wypisze wartości zmiennych na konsoli. Możesz to zrobić dla pojedynczej zmiennej, jak i dla większej ilości zależnych od siebie zmiennych.

```C#
int liczba = 5;
string napis = "To jest przykładowy napis.";

Console.WriteLine("Liczba: " + liczba);
Console.WriteLine("Napis: " + napis);
```

Output:
```
Liczba: 5
Napis: To jest przykładowy napis.
```

Pamiętaj, aby używać znaku `+` w miejscu, w którym chcesz dodać wartości zmiennych, a także używać znaku `;` na końcu każdej linii kodu.

## Deep Dive

Wypisywanie debugujące jest często używane podczas debugowania kodu, czyli procesu znajdowania i naprawiania błędów w programach. Pozwala ono na wyświetlanie aktualnych wartości zmiennych w celu monitorowania ich wartości i porównywania z oczekiwanymi. Dzięki temu możemy łatwiej zlokalizować błąd i naprawić go.

Ponadto, wypisywanie debugujące może pomóc w zrozumieniu działania kodu, szczególnie dla początkujących programistów. Pozwala na śledzenie przepływu wartości i zmian w zmiennych, co może ułatwić zrozumienie działania programu.

## Zobacz Również

Jeśli chcesz dowiedzieć się więcej o wypisywaniu debugującym w C#, polecam zapoznać się z poniższymi linkami:

- [Oficjalna dokumentacja C# o wypisywaniu debugującym](https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/inside-a-program/using-the-console-class)
- [Poradnik na temat wypisywania debugującego w C#](https://www.c-sharpcorner.com/article/debugging-in-C-Sharp/)
- [Wideo tutorial o wypisywaniu debugującym w C#](https://www.youtube.com/watch?v=bOhtuU-AuZQ)