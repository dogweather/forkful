---
title:    "C#: Drukowanie wyników debugowania"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu napotykamy problemy, których nie jesteśmy w stanie rozwiązać od razu. W takich sytuacjach, wyświetlanie informacji debuggowania może być nieocenionym narzędziem, pozwalającym na znalezienie błędu w aplikacji.

## Jak

Aby wyświetlać informacje debugowania w języku C#, możemy skorzystać z polecenia "Console.WriteLine()", które wypisze przekazaną mu wartość na standardowe wyjście konsoli. Możemy to wykorzystać na przykład do wyświetlenia wartości zmiennych w trakcie działania programu.

```C#
int liczba1 = 5;
int liczba2 = 10;

Console.WriteLine("Wartość zmiennej liczba1: " + liczba1); // wypisze: "Wartość zmiennej liczba1: 5"
Console.WriteLine("Wartość zmiennej liczba2: " + liczba2); // wypisze: "Wartość zmiennej liczba2: 10"

```

Jeśli chcemy wypisać więcej informacji na temat stanu programu, możemy również wykorzystać metody takie jak "Console.Write()" lub "Debug.WriteLine()".

```C#
string imie = "Anna";
int wiek = 28;

Console.Write("Użytkownik: ");
Console.WriteLine(imie); // wypisze: "Użytkownik: Anna"
Debug.WriteLine("Wiek: " + wiek); // wypisze: "Wiek: 28"
```

Dzięki wyświetlaniu informacji debugowania możemy śledzić, w jaki sposób zmienne zmieniają swoją wartość, lub zlokalizować miejsce, w którym program zwraca niepoprawne wyniki.

## Deep Dive

Istnieje także możliwość korzystania z różnych ustawień i opcji dla wyświetlania informacji debugowania. Na przykład w Visual Studio, możemy skorzystać z okna "Debugowanie", które pozwala na wyświetlenie zmiennych, stanu stosu, bądź wywołań dla aktualnie wykonywanej funkcji.

Ponadto, możemy wykorzystać specjalną bibliotekę "System.Diagnostics" do bardziej zaawansowanego ustawiania i kontrolowania wyświetlanych informacji.

## Zobacz także

- Microsoft Dokumentacja: https://docs.microsoft.com/pl-pl/visualstudio/debugger/getting-started-with-the-debugger?view=vs-2019
- Wprowadzenie do wyświetlania informacji debugowania: https://www.c-sharpcorner.com/blogs/guide-to-viewing-debug-information-in-c-sharp-programming1
- Debugowanie aplikacji w języku C#: https://www.c-sharpcorner.com/UploadFile/5c5641/5-steps-to-debug-an-application-in-C-Sharp/