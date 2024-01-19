---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwersja daty na ciąg znaków, zwany też stringiem, to proces zamieniania daty na tekst. Programiści często posługują się nią do wyświetlania czy zapisywania dat w sposób zrozumiały dla użytkownika.

## Jak to zrobić:

W C# do konwersji daty na ciąg znaków możemy użyć metody `ToString()`, która jest wbudowana w typ `DateTime`. 

```C#
DateTime teraz = DateTime.Now;
string dataJakoString = teraz.ToString();

Console.WriteLine(dataJakoString);
```
Przykładowe wyjście dla powyższego kodu to `09/09/2022 11:45:22 AM`.

Możemy także określić format tego ciągu danych, podając go jako argument metody `ToString()`.

```C#
DateTime teraz = DateTime.Now;
string dataJakoString = teraz.ToString("dd-MM-yyyy");

Console.WriteLine(dataJakoString);
```
Teraz nasza data wygląda tak: `09-09-2022`.

## Głębsze spojrzenie:

Początki konwersji dat na stringi sięgają momentu, gdy komputery zaczęły przechowywać daty jako liczby, a następnie potrzebowały je zamieniać na tekst dla czytelności użytkownika. 

Jest wiele alternatyw dla metody `ToString()`. W C# możemy używać metod jak `ToShortDateString()` czy `ToLongDateString()` dla bardziej specyficznych formatów. 

Ważne jest również zauważenie, że `ToString()` korzysta z kultury (ustawień regionalnych) systemu do formatowania danych. Możemy zdefiniować własną kulturę za pomocą klasy `CultureInfo`. 

## Zobacz także:

1. [Przekształcanie dat na ciągi — Microsoft Docs](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.tostring?view=net-5.0)
2. [Kultura w C# — Microsoft Docs](https://docs.microsoft.com/pl-pl/dotnet/api/system.globalization.cultureinfo?view=net-5.0) 
3. [Formatowanie dat i czasu — Microsoft Docs](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/formatting-types)