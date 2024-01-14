---
title:                "C#: Wydrukowanie wyników debugowania"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania programów w C#, napotykamy się na różne problemy i błędy. Wtedy niezwykle przydatne jest wypisywanie informacji debugowania. To pozwala nam na śledzenie przebiegu programu i zlokalizowanie potencjalnych problemów. W tym artykule opiszemy dlaczego warto korzystać z drukowania danych debugowania i w jaki sposób to zrobić.

## Jak

Aby wypisywać dane debugowania w C#, należy skorzystać z metody `Console.WriteLine()`. Przyjmujemy do niej argumenty, które chcemy wyświetlić, rozdzielając je przecinkami. 

```C#
int age = 25;
string name = "Anna";

// Wyświetlenie danych debugowania
Console.WriteLine("Wiek: " + age + ", Imię: " + name);

/* Output:
Wiek: 25, Imię: Anna
*/
```

Możemy także korzystać z zapisu interpolowanego, dodając znak dolara przed cudzysłowem i umieszczając w nim zmienną w klamrach. Ta metoda jest szczególnie przydatna przy wyświetlaniu większej ilości zmiennych.

```C#
int age = 25;
string name = "Anna";

// Wyświetlenie danych debugowania
Console.WriteLine($"Wiek: {age}, Imię: {name}");

/* Output:
Wiek: 25, Imię: Anna
*/
```

Dodatkowo, jeśli chcemy wypisać wartość zmiennej w systemowym formacie, możemy użyć metody `ToString()` z opcjonalną specyfikacją formatowania.

```C#
decimal price = 12.34M;

// Wyświetlenie danych debugowania z użyciem specyfikacji formatowania
Console.WriteLine("Cena: " + price.ToString("C")); // formatowanie jako waluta
Console.WriteLine("Cena: " + price.ToString("N2")); // 2 miejsca po przecinku

/* Output:
Cena: $12.34
Cena: 12.34
*/
```

## Deep Dive

Drukowanie danych debugowania jest nie tylko przydatne w odnajdywaniu błędów, ale także w monitorowaniu programu podczas jego działania. Możemy wyświetlać wartości zmiennych w różnych punktach programu, aby upewnić się, że wszystko przebiega zgodnie z oczekiwaniami.

Dodatkowo, w przypadku użycia kodu wielowątkowego, drukowanie danych debugowania może stanowić nieocenione narzędzie do śledzenia przebiegu różnych wątków i wykrywania ewentualnych problemów z synchronizacją.

Podczas pisania kodu, warto także wypisywać informacje o fakcie wykonania konkretnych funkcji czy metod. Dzięki temu możemy śledzić, które części programu zostały już wykonane, a które jeszcze nie.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o drukowaniu danych debugowania w C#, polecamy przeczytać poniższe artykuły:

- [Debugowanie aplikacji .NET Core z Visual Studio Code](https://docs.microsoft.com/pl-pl/dotnet/core/tutorials/debugging-with-visual-studio-code)
- [Drukowanie danych debugowania w programie Visual Studio](https://docs.microsoft.com/pl-pl/visualstudio/debugger/how-to-use-the-debugger-window?view=vs-2019)
- [Używanie polecenia Debug.WriteLine w aplikacjach .NET](https://docs.microsoft.com/pl-pl/dotnet/api/system.diagnostics.debug.writeline?view=net-5.0)