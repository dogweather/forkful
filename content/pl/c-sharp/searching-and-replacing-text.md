---
title:    "C#: Wyszukiwanie i zamienianie tekstu"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek musiałeś zmienić wiele fragmentów tekstu w swoim kodzie programu? Zamiast ręcznie edytować każde wystąpienie, czy nie byłoby łatwiej użyć jednej funkcji, która automatycznie wykonuje to za Ciebie? W tym wpisie dowiesz się, jak wykorzystać funkcję "search and replace" w języku C# do szybkiego i efektywnego zmieniania tekstu w swoim kodzie.

## Jak to zrobić

Aby wykorzystać funkcję "search and replace" w C#, potrzebujemy dwóch parametrów: wyrażenia regularnego i nowego tekstu. Oto przykładowy kod, który zamienia wszystkie wystąpienia słowa "kot" na "pies" w zmiennej "text":

```C#
string text = "Uwielbiam mojego kota, ale nie lubię jego łap w domu.";
string newText = Regex.Replace(text, "kot", "pies", RegexOptions.IgnoreCase);

Console.WriteLine(newText);
```

Ten kod używa funkcji `Regex.Replace`, która przyjmuje cztery parametry: tekst, wyrażenie regularne, nowy tekst i opcje. W powyższym przykładzie użyliśmy opcji `IgnoreCase`, aby funkcja ignorowała wielkość liter. Wynik działania tego kodu będzie brzmiał:

```
Uwielbiam mojego psa, ale nie lubię jego łap w domu.
```

Możemy również wykorzystać funkcję `Regex.Matches`, aby znaleźć wszystkie wystąpienia wybranego tekstu w zmiennej i zastosować na nich pętlę `foreach` w celu dokonania zmiany. Na przykład, poniższy kod znajdzie wszystkie cyfry w tekście i zamieni je na gwiazdki:

```C#
string text = "moje hasło to 12345";
string pattern = @"\d";

foreach (Match match in Regex.Matches(text, pattern))
{
    text = text.Replace(match.Value, "*");
}

Console.WriteLine(text);
```

Efektem tego kodu będzie "moje hasło to *****".

## Deep Dive

Funkcje "search and replace" są niezwykle przydatne w programowaniu, ponieważ pozwalają nam szybko i łatwo dokonać zmian w dużych ilościach tekstu. Wyrażenia regularne (regular expressions) są bardzo potężnym narzędziem, które pozwala nam precyzyjnie wybrać fragmenty tekstu do zmiany. Istnieje wiele opcji, które można wykorzystać wraz z funkcjami `Regex.Replace` i `Regex.Matches`, a najlepszym sposobem na nauczenie się ich działania jest przez eksperymentowanie i praktykę.

## Zobacz również

* [Dokumentacja C# - RegEx.Replace](https://docs.microsoft.com/pl-pl/dotnet/api/system.text.regularexpressions.regex.replace?view=net-5.0)
* [Dokumentacja C# - RegEx.Matches](https://docs.microsoft.com/pl-pl/dotnet/api/system.text.regularexpressions.regex.matches?view=net-5.0)
* [Kurs online o wyrażeniach regularnych w C#](https://www.udemy.com/course/csharp-regular-expressions/)