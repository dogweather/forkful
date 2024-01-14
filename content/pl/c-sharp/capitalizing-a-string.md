---
title:    "C#: Zapisywanie napisu dużymi literami"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego warto używać funkcji "string.ToUpper()" w języku C#?

Często zdarza się, że w trakcie programowania musimy zmienić tekst z małych liter na wielkie lub odwrotnie. W takich przypadkach przydatna jest funkcja "string.ToUpper()", która pozwala na łatwe i szybkie zmienianie wielkości liter w danym stringu.

## Jak używać funkcji "string.ToUpper()" w języku C#?

Aby użyć funkcji "string.ToUpper()", należy podać jako argument tekst, który chcemy zmienić na wielkie litery. Na przykład:

```C#
string text = "programowanie w C#";
string capitalizedText = text.ToUpper();
Console.WriteLine(capitalizedText);
```

Ten kod zwróci na konsoli tekst "PROGRAMOWANIE W C#".

Możemy również wykorzystać funkcję "string.ToUpper()" przy wprowadzaniu danych przez użytkownika, aby zapewnić jednolitą formę wpisanego tekstu.

```C#
Console.WriteLine("Podaj swoje imię:");
string name = Console.ReadLine();
string formattedName = name.ToUpper();
Console.WriteLine("Witaj, " + formattedName + "!");
```

Teraz użytkownik może wpisać swoje imię z małych lub wielkich liter, a program zawsze wyświetli je w formacie z dużymi literami. To tylko jedno z wielu przykładów wykorzystania funkcji "string.ToUpper()" w programowaniu.

## Głębsze spojrzenie na funkcję "string.ToUpper()"

Podczas zmieniania wielkości liter, funkcja "string.ToUpper()" korzysta z aktualnego ustawienia języka systemu operacyjnego. Jeśli chcemy, aby tekst zmieniał się zgodnie z innym językiem, możemy przekazać go jako drugi argument do funkcji. Na przykład:

```C#
string text = "uxe z małych na wielkie";
string capitalText = text.ToUpper(new CultureInfo("pl-PL"));
Console.WriteLine(capitalText);
```

Ten kod zwróci "UXE Z MAŁYCH NA WIELKIE", ponieważ użyliśmy polskiego języka jako drugiego argumentu.

Funkcja "string.ToUpper()" także uwzględnia polskie znaki diakrytyczne, takie jak ą, ę, ć itp. Dzięki temu możemy zmienić wielkość liter bez obaw o utratę polskich znaków.

## Zobacz też

- Dokumentacja na temat funkcji "string.ToUpper()": https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper
- Porównanie innych funkcji do zmiany wielkości liter w języku C#: https://www.c-sharpcorner.com/article/string-manipulation-methods-in-C-Sharp/
- Przykłady użycia funkcji "string.ToUpper()" w praktyce: https://www.codegrepper.com/code-examples/csharp/c%23+toUpper