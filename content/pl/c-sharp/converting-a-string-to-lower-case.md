---
title:                "Zmiana ciągu znaków na małe litery"
html_title:           "C#: Zmiana ciągu znaków na małe litery"
simple_title:         "Zmiana ciągu znaków na małe litery"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwersja ciągu znaków na małe litery jest procesem zmiany wszystkich liter w ciągu na małe litery. Programiści wykonują tę operację, ponieważ ułatwia to porównywanie ciągów znaków i manipulowanie nimi w kodzie.

## Jak to zrobić:

```C#
string exampleString = "PRZYKŁADOWY CIĄG ZNAKÓW";

string lowerCaseString = exampleString.ToLower();

Console.WriteLine(lowerCaseString); 
// Output: przykładowy ciąg znaków
```

## Wszystko na głębszym poziomie:

Konwersja ciągu znaków na małe litery jest standardową funkcją dostępną w większości języków programowania. Ma ona swoje korzenie w tradycji pisania kodu, gdzie użycie małych liter ułatwiało czytelność i zwiększało wydajność kodu. Alternatywnymi sposobami na zmianę ciągu na małe litery jest użycie funkcji `ToLowerInvariant()` lub `ToLower()` z określonym językiem kulturowym. Implementacja konwersji małych liter różni się w zależności od języka programowania, ale ogólnie polega ona na zamianie kodu znaków na kody odpowiadających im małych liter.

## Zobacz również:

- [Dokumentacja Microsoft o metodzie ToLower()](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [Wyjaśnienie różnicy między metodami ToLower() i ToLowerInvariant()](https://stackoverflow.com/questions/1030070/string-tolower-vs-string-tolowerinvariant)