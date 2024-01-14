---
title:    "C#: Wykorzystywanie wyrażeń regularnych"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego używać wyrażeń regularnych?

Wyrażenia regularne, zwane również regexami, są potężnym narzędziem w programowaniu, które pomagają w przetwarzaniu tekstów i wyszukiwaniu wyrażeń dopasowujących do określonych wzorców. Używanie wyrażeń regularnych pozwala zaoszczędzić czas i wysiłek przy przetwarzaniu danych tekstowych. Jest to szczególnie przydatne w przypadku analizy dużych ilości informacji lub w przypadku gdy tekst ma różne formatowanie.

## Jak używać wyrażeń regularnych w C#?

Język C# zapewnia biblioteki i narzędzia do wykorzystania wyrażeń regularnych. Poniżej znajduje się przykładowy kod, który wyjaśni jak używać wyrażeń regularnych w C#:

```C#
string input = "To jest przykładowy tekst do przetworzenia."
string pattern = "^To jest (.*) tekst do przetworzenia.$"; // wzorzec dopasowujący tekst między "To jest" a "tekst do przetworzenia"
Match match = Regex.Match(input, pattern); // dopasowanie tekstu do wzorca
if (match.Success)
{
    Console.WriteLine(match.Groups[1].Value); // wyświetlenie dopasowanego tekstu
}
```

```
Output:
przykładowy
```

Możemy również użyć wyrażeń regularnych do wykrywania i zamiany tekstów. Na przykład, możemy chcieć zmienić format numeru telefonu z "123-456-7890" na "(123) 456-7890". W tym celu możemy użyć następującego kodu:

```C#
string input = "123-456-7890";
string pattern = "(\\d{3})-(\\d{3})-(\\d{4})"; // wzorzec dopasowujący trzy cyfry, myślnik, trzy cyfry, myślnik, cztery cyfry
string replacement = "($1) $2-$3"; // znaki z grupy odnoszą się do kolejności dopasowanych wyrażeń w wzorcu
string result = Regex.Replace(input, pattern, replacement); // zamiana dopasowanego tekstu według wzorca
Console.WriteLine(result);
```

```
Output:
(123) 456-7890
```

## Głębsze zanurzenie w używaniu wyrażeń regularnych

Wyrażenia regularne w C# posiadają wiele zaawansowanych funkcji, takich jak metaznaki, grupowanie lub wyrażenia regularne wielowierszowe, które pozwalają na jeszcze bardziej zaawansowane przetwarzanie tekstu. Warto poświęcić czas na naukę tych możliwości, ponieważ może to znacznie przyspieszyć pracę z tekstem w programowaniu.

## Zobacz też

- [Dokumentacja C# - Wyrażenia regularne](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/regular-expressions) 
- [Tutorial wyrażeń regularnych w C#](https://www.dotnetperls.com/regex)
- [Lista wyrażeń regularnych w C#](https://www.rexegg.com/regex-csharp.html)