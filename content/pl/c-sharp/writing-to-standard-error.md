---
title:    "C#: Pisanie do standardowego błędu"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Dlaczego?

Czasami, kiedy piszesz programy w języku C#, możliwe jest, że będziesz chciał pisać do standardowego błędu zamiast do standardowego wyjścia. Jest to przydatne, gdy chcesz przekazać informacje o błędach lub ostrzeżenia użytkownikom twojego programu.

## Jak to zrobić?

Aby wypisać coś do standardowego błędu, musisz skorzystać z konstrukcji ```Console.Error.WriteLine("Tekst do wypisania");```. Na przykład, jeśli chcesz wypisać błąd, możesz użyć kodu:

```C#
try
{
    // kod, który może wywołać błąd
}
catch (Exception ex)
{
    Console.Error.WriteLine("Wystąpił błąd: " + ex.Message);
}
```

Ten kod wypisze komunikat o błędzie wraz z informacją o samym błędzie.

## Głębszy zanurzenie

Wypisywanie do standardowego błędu może być szczególnie przydatne w przypadku obsługi wyjątków. Możesz wtedy wypisać informacje o błędzie, które mogą pomóc w jego zrozumieniu i naprawieniu. Możesz również użyć tej konstrukcji w celu wypisania ostrzeżeń lub innych komunikatów dla użytkownika.

Należy pamiętać, że jeśli nie zostanie przechwycony żaden wyjątek, to wypisywanie do standardowego błędu nie będzie miało żadnego efektu.

## Zobacz również

- [Dokumentacja Microsoft o klasie Console](https://docs.microsoft.com/en-us/dotnet/api/system.console?view=net-5.0)
- [Dlaczego warto używać standardowego błędu w swoim kodzie? (wideo)](https://www.youtube.com/watch?v=9zF5QkZKIjg)
- [Przykładowy kod z użyciem wypisywania do standardowego błędu](https://www.tutorialspoint.com/csharp/csharp_exceptions.htm)