---
title:    "C#: Zmiana ciągu znaków na małe litery"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Dlaczego?

Często zdarza się, że w trakcie pisania kodu musimy zmienić duże litery w ciągu znaków na małe. Może to być potrzebne do porównywania danych lub wyświetlania tekstu w jednolitym formacie. W takich przypadkach przydatną umiejętnością jest konwertowanie ciągów na małe litery w języku C#.

## Jak to zrobić?

W języku C# istnieje specjalna metoda o nazwie ToLower(), która pozwala na zmianę ciągu znaków na małe litery. Oto prosty przykład kodu, który wykorzystuje tę funkcję:

```C#
string text = "CIASTECZKA SĄ NAJLEPSZE!";
string lowerText = text.ToLower();

Console.WriteLine(lowerText);
```

Wynik powyższego kodu będzie wyglądał następująco:

```sh
ciasteczka są najlepsze!
```

Jak widać, za pomocą jednej metody udało nam się zmienić wszystkie duże litery na małe. Jest to bardzo proste, a jednocześnie bardzo wygodne rozwiązanie.

Warto również zauważyć, że funkcja ToLower() działa dla różnych języków, co oznacza, że nie musimy martwić się o zmianę liter w wybranym języku.

## Deep Dive

Konwersja ciągu na małe litery może wydawać się banalna, ale warto dowiedzieć się, jak dokładnie działa ta funkcja. W języku C# ciągi znaków są traktowane jako tablice typu char. Każdy znak w ciągu ma swój własny kod ASCII lub Unicode. Funkcja ToLower() działa w oparciu o te kody, zamieniając duże litery na ich odpowiedniki w postaci małych liter.

Warto również zauważyć, że funkcja ToLower() zwraca nowy ciąg znaków, a nie zmienia istniejącego. Dzięki temu mamy pewność, że oryginalny ciąg pozostaje niezmieniony.

## Zobacz także

- [Dokumentacja języka C# on Microsoft Docs](https://docs.microsoft.com/pl-pl/dotnet/csharp/)

- [Poradnik programisty C# on YouTube](https://www.youtube.com/watch?v=N1Lq1_WNKz8)

- [Przewodnik dla początkujących w języku C# on Codecademy](https://www.codecademy.com/learn/learn-c-sharp)