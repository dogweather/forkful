---
title:                "Używanie wyrażeń regularnych"
html_title:           "C#: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Czemu

 Jeśli jesteś programistą C#, istnieje duże prawdopodobieństwo, że w pewnym momencie będziesz potrzebował wykorzystać wyrażenia regularne w swoim kodzie. Mogą one być bardzo przydatne do wyszukiwania, porównywania i przetwarzania danych tekstowych.

## Jak używać

Wykorzystanie wyrażeń regularnych w C# jest proste i nie wymaga dużo kodu. Po prostu dodaj poniższą linię do początku pliku:
```
using System.Text.RegularExpressions
```
Następnie możesz tworzyć wyrażenia regularne za pomocą klasy `Regex`. Na przykład, jeśli chcesz sprawdzić czy dany ciąg tekstowy zawiera tylko liczby, możesz użyć następującego wyrażenia:
```
Regex.IsMatch("12345", "^[0-9]+$"); // true
Regex.IsMatch("abc123", "^[0-9]+$"); // false
```
Możesz również wykorzystać wyrażenia regularne do przetwarzania tekstu. Na przykład, jeśli chciałbyś zastąpić wszystkie wystąpienia słowa "pies" w tekście na "kot", możesz wykorzystać metodę `Regex.Replace()`:
```
string text = "Lubię psy, ale mój znajomy nie.";
string newText = Regex.Replace(text, "pies", "kot");
Console.WriteLine(newText); // "Lubię koty, ale mój znajomy nie."
```

## Deep Dive

Wyrażenia regularne w C# dają wiele możliwości, takich jak wykorzystanie tzw. wyrażeń grupujących, które pozwolą na wyciągnięcie konkretnej wartości z tekstu. Na przykład, możesz w ten sposób wyciągnąć numer telefonu z ciągu tekstowego:
```
string text = "Mój numer telefonu to 111-222-3333.";
string pattern = @"\b[0-9]{3}-[0-9]{3}-[0-9]{4}\b";
Match match = Regex.Match(text, pattern);
if (match.Success)
{
    string phoneNumber = match.Value; // "111-222-3333"
}
```
Możesz także wykorzystać wyrażenia regularne do walidacji adresów email, identyfikatorów, numerów ID i wielu innych. Warto zaznaczyć, że wyrażenia regularne mogą być często używane do złożonych zadań, jednak przy dużych i skomplikowanych wyrażeniach, mogą one spowodować znaczne spowolnienie wydajności aplikacji.

## Zobacz także

- [Microsoft Docs: Wyrażenia regularne w C#](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Codecademy: Wyrażenia regularne w C#](https://www.codecademy.com/learn/learn-regular-expressions/modules/learn-regular-expressions-dotnet/cheatsheet)
- [Regex101: Testowanie i sprawdzanie wyrażeń regularnych](https://regex101.com/)