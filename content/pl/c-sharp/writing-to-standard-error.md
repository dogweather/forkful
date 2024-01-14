---
title:    "C#: Pisanie do standardowego błędu."
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego pisać do standardowego wyjścia błędów

Pisanie do standardowego wyjścia błędów może być bardzo przydatne podczas pisania programów w języku C#. Często występują różne problemy podczas wykonywania kodu i konieczne jest szybkie znalezienie źródła błędu. Właśnie w takich sytuacjach pisanie do standardowego wyjścia błędów może ułatwić nam zlokalizowanie problemu i szybkie jego naprawienie.

## Jak to zrobić?

Aby napisać do standardowego wyjścia błędów w języku C#, użyjemy prostego wywołania funkcji `Console.Error.WriteLine()`. Jako argument podajemy wiadomość, którą chcemy wyświetlić. Spróbujmy tego przykładu:

```C#
Console.Error.WriteLine("To jest informacja o błędzie!");
```

Wykonanie tego kodu spowoduje wyświetlenie tekstu "To jest informacja o błędzie!" w standardowym wyjściu błędów.

Oto przykładowe wyjście:

```
To jest informacja o błędzie!
```

## Głębsze omówienie

Pisanie do standardowego wyjścia błędów jest szczególnie przydatne w przypadku aplikacji, które działają w trybie konsolowym. Dzięki temu możemy łatwo monitorować wykonywanie kodu i szybko reagować na ewentualne błędy.

Warto też pamiętać, że możemy przekierować standardowe wyjście błędów do pliku lub innego urządzenia, co może być wygodne w niektórych sytuacjach. Aby to zrobić, możemy użyć strumienia `System.Console.Error` i przypisać go do innej zmiennej, takiej jak `System.IO.FileStream`.

## Zobacz także

Poniżej znajduje się lista przydatnych linków dotyczących pisania w języku C#:

- [Dokumentacja języka C#](https://docs.microsoft.com/pl-pl/dotnet/csharp/)
- [Kurs języka C# na platformie Codecademy](https://www.codecademy.com/learn/learn-c-sharp)
- [Forum dla programistów w języku C#](https://forum.pasja-informatyki.pl/Forum-C-sharp)