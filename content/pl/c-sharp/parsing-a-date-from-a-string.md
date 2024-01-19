---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Przetwarzanie daty z łańcucha znaków to proces konwersji tekstów na datę. To jest kluczowe dla programistów, gdyż pozwala manipulować i używać danych czasu które są zapisane jako tekst.

## Jak to zrobić:
Praca z datami w C# jest prosta dzięki wbudowanej klasie DateTime. Poniżej znajduje się przykład konwersji łańcucha na datę:

```C#
string myDateString = "2022-09-22 14:30";
DateTime dateVar = DateTime.Parse(myDateString);
Console.WriteLine(dateVar);
```
W wyniku otrzymamy `2022-09-22 14:30:00`

Jednakże, jeżeli twój łańcuch daty ma niestandardowy format, musisz użyć `DateTime.ParseExact()`.

```C#
string dateString = "22/09/2022 14:30";
string format = "dd/MM/yyyy HH:mm";
DateTime resultDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);
Console.WriteLine(resultDate);
```

Po wykonaniu tego kodu wynik będzie `2022-09-22 14:30:00`.

## Głębsze zanurzenie
Początkowo, C# nie zawierał funkcji parsowania daty. Dopiero C# 2.0 wprowadził `DateTime.Parse()`. Następnie w C# 3.5, aby sprostać rosnącemu zapotrzebowaniu na elastyczność, dodano `DateTime.ParseExact()`.

Alternatywą dla `DateTime.Parse()` jest `DateTime.TryParse()`. Ta metoda zwraca wartość logiczną, która informuje, czy konwersja się powiodła czy nie, co pozwala łatwiej obsłużyć nieoczekiwane formaty.

Szczególną rzeczą, na którą należy zwrócić uwagę, jest to, że metoda `DateTime.Parse()` będzie korzystać z ustawień regionalnych systemu do interpretacji tekstu. Jeżeli potrzebujesz konkretnego formatu, skorzystaj z `DateTime.ParseExact()` i określ odpowiedni format.

## Zobacz też
Istnieje wiele źródeł, które pomogą ci zrozumieć ten temat:

1. [Dokumentacja Microsoft na temat klasy DateTime](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime)
2. [Przewodnik Microsoft na temat formatowania i analizowania dat](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/formatting-and-parsing-dates-and-times)
3. [Dokumentacja Microsoft na temat metody ParseExact()](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.parseexact)