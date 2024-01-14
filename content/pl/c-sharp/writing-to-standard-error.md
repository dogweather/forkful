---
title:                "C#: Pisanie do standardowego błędu"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego pisać na błąd standardowy?

Pisanie informacji na błąd standardowy jest ważną częścią procesu programowania w języku C#. Wydaje się to być niepozornym krokiem, ale może bardzo ułatwić debugowanie i monitorowanie działania programu.

## Jak to zrobić?

Język C# posiada prostą funkcję do wypisywania informacji na błąd standardowy - `Console.Error.WriteLine()`. Za pomocą tej funkcji możemy wyświetlić dowolną informację na konsoli błędów. Przyjrzyjmy się przykładowemu kodowi:

```C#
string name = "Jan";
Console.Error.WriteLine("Witaj, " + name + "!");
```

Wyjście powyższego kodu będzie wyglądało następująco:

`Witaj, Jan!`

Pamiętajmy, że funkcja `Console.Error.WriteLine()` wymaga dodania znaku `+` pomiędzy tekstem a zmienną, aby wszystko zostało wyświetlone poprawnie.

## Głębszy wgląd

Pisanie informacji na błąd standardowy jest szczególnie przydatne w sytuacjach, gdy musimy śledzić działanie naszego programu. Możemy wykorzystać tę funkcję do wyświetlania informacji o aktualnie przetwarzanych danych, bądź też do informowania o znalezionych błędach. Ponadto, możemy także przekierować konsolę błędów do pliku, co ułatwi nam analizowanie i debugowanie programu.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o pisaniu na błąd standardowy w języku C#, zapoznaj się z poniższymi materiałami:

- [Konsola błędów w C# (Microsoft Docs)](https://docs.microsoft.com/pl-pl/dotnet/api/system.console.error)
- [Tworzenie i zapisywanie do plików tekstowych w C# (C# Tutorials)](https://csharp.net-tutorials.com/intermediate/file-i-o/reading-and-writing/)

Miej na uwadze, że pisanie na błąd standardowy nie jest jedynym sposobem na wyświetlanie informacji w trakcie działania programu. Zawsze dobrym pomysłem jest skorzystanie z debugera, który pozwoli nam prześledzić dokładnie, co dzieje się w naszym kodzie.