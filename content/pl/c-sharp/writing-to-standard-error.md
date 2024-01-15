---
title:                "Pisanie do wyjścia błędu standardowego"
html_title:           "C#: Pisanie do wyjścia błędu standardowego"
simple_title:         "Pisanie do wyjścia błędu standardowego"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu jest ważnym elementem procesu programowania w C#. To pozwala programistom śledzić i radzić sobie z błędami w swoim kodzie. Pozwala również na lepsze debugowanie i szukanie przyczyn problemów w aplikacjach. 

## Jak to zrobić

Aby pisać do standardowego błędu w C#, możemy użyć metody Console.Error.WriteLine (). Jest to prosta i wygodna funkcja, która pozwala na wysyłanie wiadomości do standardowego błędu.

Proszę zauważyć, że metoda ta jest używana w połączeniu z obiektem Console i jest rozróżniana od funkcji Console.WriteLine (), która pisze do standardowego wyjścia.

Poniższy przykład kodu demonstruje użycie metody Console.Error.WriteLine () w połączeniu z blokiem try-catch, aby przechwycić i wyświetlić błędy w kodzie:

```C#
try 
{
    int result = Divide(10, 0);
    Console.WriteLine("Wynik dzielenia to: " + result);
}
catch (DivideByZeroException e)
{
    Console.Error.WriteLine("Wystąpił błąd: " + e.Message);
}
```

Wynik działania powyższego kodu będzie wyglądał następująco w konsoli:

```
Wystąpił błąd: Dzielenie przez zero jest niedozwolone.
```

Zauważ, że wiadomość błędu została wypisana do standardowego błędu za pomocą metody Console.Error.WriteLine ().

## Deep Dive

Wraz z metodą Console.Error.WriteLine (), istnieje także funkcja Console.Error.Write (), która działa w podobny sposób, ale nie dodaje znaku nowej linii na końcu wiadomości. Ten mały szczegół może być przydatny w niektórych przypadkach, gdy chcemy kontrolować formatowanie wypisywanego tekstu.

Ponadto, warto nadmienić, że zarówno metoda Console.Error.WriteLine (), jak i Console.Error.Write () mogą przyjmować argumenty w różnych formatach, takich jak stringi, liczby czy zmienne. Możliwość wykorzystania różnych typów danych pozwala na większą elastyczność w pisaniu do standardowego błędu.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o obsłudze błędów w C#, możesz przeczytać następujące artykuły:

- [Obsługa błędów w C#](https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/exceptions/)
- [Wyjątki w C#](https://www.c-sharpcorner.com/UploadFile/dbd951/exceptions-in-C-Sharp/)

Możesz także zapoznać się z dokumentacją Microsoftu na temat klasy Console, która jest używana w przykładach powyżej:

- [Klasa Console w C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.console)